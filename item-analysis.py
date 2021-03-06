#!/usr/bin/env python2
import sys, bz2, time, gzip, os, urllib, re
import json
from collections import defaultdict
# This scripts creates the knowledge base and collects a few numbers by going through
# all dailies up to and the latest available dump. This usually runs a few hours.

def log(txt) :
	print txt

log('Calculating Wikidata stats')

start_time = time.time()

# for dictionary creation
langs = [ 'en', 'de', 'hr', 'uz' ]

# read the list of bots
log('Loading list of bots')
bots = []
botsjson = urllib.urlopen('http://www.wikidata.org/w/api.php?action=query&list=allusers&augroup=bot&aulimit=500&format=json').read()
botsjson = eval(botsjson)
for bot in botsjson['query']['allusers'] :
	bots.append(bot['name'])
log('List of bots: ' + str(bots))

linecount = 0
charactercount = 0

# Items
itemcount = 0
itemswithclaims = 0
claimcount = 0
claimsperitem = {}
claimswithrefs = 0
refs = 0
itemswithrefs = 0
labelcount = 0
descriptioncount = 0
sitelinkcount = 0
itemrevisioncount = 0
botrevisioncount = 0
revisionsperitem = {}
titleofmostclaims = ''
langlabels = {}
langdescriptions = {}
langsitelinks = {}

# Properties
propertycount = 0
propertylabelcount = 0
propertydescriptioncount = 0

# General
pagecount = 0
revisioncount = 0
processedpages = set()
processedrevisions = set()

#langsofitem
langsofitem = defaultdict(list)

# if there is no data directory, create one
if not os.path.exists('data') :
	os.makedirs('data')
os.chdir('data')

# download the dumps directory file and figure out the date of the latest dump
log('Checking for the date of the last dump')
latestdump = '20121026'
for line in urllib.urlopen('http://dumps.wikimedia.org/wikidatawiki/') :
	if not line.startswith('<tr><td class="n">') : continue
	date = line[27:35]
	if not re.match('\d\d\d\d\d\d\d\d', date) : continue
	log("Checking dump of " + date)
	# check if dump is finished
	finished = False
	for md5 in urllib.urlopen('http://dumps.wikimedia.org/wikidatawiki/' + date + '/wikidatawiki-' + date + '-md5sums.txt') :
		if md5.endswith('-pages-meta-history.xml.bz2' + "\n") :
			finished = True
	if finished :
		latestdump = date
log('Latest dump has been on ' + latestdump)
#latestdump = '20130417'

# download the latest stats if needed
if not os.path.exists('dump' + latestdump) :
	os.makedirs('dump' + latestdump)
os.chdir('dump' + latestdump)
if not os.path.exists('site_stats.sql.gz') :
	log('Downloading stats of the latest dump')
	urllib.urlretrieve('http://dumps.wikimedia.org/wikidatawiki/' + latestdump + '/wikidatawiki-' + latestdump + '-site_stats.sql.gz', 'site_stats.sql.gz')
	
# download the latest dump if needed
if not os.path.exists('pages-meta-history.xml.bz2') :
	log('Downloading latest dump')
	urllib.urlretrieve('http://dumps.wikimedia.org/wikidatawiki/' + latestdump + '/wikidatawiki-' + latestdump + '-pages-meta-history.xml.bz2', 'pages-meta-history.xml.bz2')

# get the maxrevid of the latest dump
maxrevid = 0
for line in gzip.open('site_stats.sql.gz'):
	if not line.startswith('INSERT INTO') : continue
	stats = eval(line[32:-2])
	maxrevid = int(stats[2])
log('maxrevid of the latest dump: ' + str(maxrevid))

os.chdir('..')

# check the dailies
dailies = []
for line in urllib.urlopen('http://dumps.wikimedia.org/other/incr/wikidatawiki/') :
	if not line.startswith('<tr><td class="n">') : continue
	date = line[27:35]
	if not re.match('\d\d\d\d\d\d\d\d', date) : continue
	dailies.append(date)

# download the dailies in reversed order until the daily maxrevid is smaller than our maxrevid
stopdaily = '20121026'
lastdaily = 0
for daily in reversed(dailies) :
	log('Checking daily of ' + daily)
	if not os.path.exists('daily' + daily) :
		os.makedirs('daily' + daily)
	os.chdir('daily' + daily)
	if not os.path.exists('maxrevid.txt') :
		urllib.urlretrieve('http://dumps.wikimedia.org/other/incr/wikidatawiki/' + daily + '/maxrevid.txt', 'maxrevid.txt')
	dailymaxrevid = int(open('maxrevid.txt').read())
	if dailymaxrevid < maxrevid :
		log('Daily ' + daily + ' is within latest dump')
		stopdaily = daily
		os.chdir('..')
		break
	if not os.path.exists('pages-meta-hist-incr.xml.bz2') :
		log('Downloading daily ' + daily)
		if urllib.urlopen('http://dumps.wikimedia.org/other/incr/wikidatawiki/' + daily + '/status.txt').read() == 'done' :
			urllib.urlretrieve('http://dumps.wikimedia.org/other/incr/wikidatawiki/' + daily + '/wikidatawiki-' + daily + '-pages-meta-hist-incr.xml.bz2', 'pages-meta-hist-incr.xml.bz2')
			log('Done downloading daily ' + daily)
			if lastdaily == 0 : lastdaily = daily
		else :
			log('Daily not done yet - download aborted')
	os.chdir('..')

def processfile(file) :
	global linecount
	global charactercount

	# Items
	global itemcount
	global itemswithclaims
	global claimcount
	global claimsperitem
	global claimswithrefs
	global refs
	global itemswithrefs
	global mostclaims
	global labelcount
	global descriptioncount
	global sitelinkcount
	global itemrevisioncount
	global botrevisioncount
	global titleofmostclaims
	global langlabels
	global langdescriptions
	global langsitelinks

	# Properties
	global propertycount
	global propertylabelcount
	global propertydescriptioncount

	# General
	global revisioncount
	
	#langsofitem
	global langsofitem
	
	# local variables
	title = ''
	item = False
	property = False
	newrev = False
	newtitle = False
	val = {}
	revid = 0
	
	for line in file :
		linecount += 1
		charactercount += len(line)
		if linecount % 1000000 == 0 : log(str(linecount / 1000000))

		# starts a new page
		if line == '  <page>\n' :
			title = ''
			item = False
			property = False
			newrev = False
			newtitle = False
			val = {}
			content = ''
			revid = 0

		if line == '    <revision>\n' :
			revid = 0

		# title
		if line.startswith('    <title>') :
			title = line[11:-9]
			item = title.startswith('Q')
			property = title.startswith('Property:P')
			if title not in processedpages :
				newtitle = True
				processedpages.add(title)
				

		if line.startswith('      <id>') :
			revid = line[10:-6]
			if revid not in processedrevisions :
				newrev = True
				processedrevisions.add(revid)

		# finished a page
		if line == '  </page>\n' :
			if not newtitle : continue

			if item:
				content = content.replace('&quot;', '"')
				val = eval(content)

				#langsofitem begin
				try:
					for langlink in val['links']:
						langsofitem[title].append(langlink[:-4]) 
				except KeyError:
					print val
					ri = raw_input()
#print langsofitem
				#ri = raw_input

		if line == '    </revision>\n' :
			if not newrev : continue
			revisioncount += 1
			if item:
				itemrevisioncount += 1
		if line.startswith('        <username>') :
			if not newrev : continue
			username = line[18:-12]
			if username in bots:
				botrevisioncount += 1
		if line.startswith('      <timestamp>') :
			timestamp = line[17:-23]

		# checks for anomalies
		if line.startswith('      <text xml:space="preserve">') :
			if item or property :
				if not line.endswith('</text>\n') :
					log(line)
				else :
					content = line[33:-8]
		#if linecount >= 1000000 : break

kb = open('kb.txt', 'w')
kb.write('# ' + str(lastdaily) + "\n")
dic = dict()
for lang in langs:
	dic[lang] = open('dict-' + lang + '.txt', 'w')

# process the dailies, starting with the newest
files = 0
for daily in reversed(dailies) :
	if daily == stopdaily : break
	log('Analysing daily ' + daily)
	os.chdir('daily' + daily)
	if not os.path.exists('pages-meta-hist-incr.xml.bz2') :
		log('No data available')
		os.chdir('..')
		continue
	file = bz2.BZ2File('pages-meta-hist-incr.xml.bz2')

	processfile(file)

	os.chdir('..')

# process the dump
log('Analysing dump ' + str(latestdump))
os.chdir('dump' + latestdump)
file = bz2.BZ2File('pages-meta-history.xml.bz2')
processfile(file)
os.chdir('..')
langsofitemfile = open('langsofitem.json', 'w')
json.dump(langsofitem, langsofitemfile, indent=4)
langsofitemfile.close()

kb.close()
for lang in langs:
	dic[lang].close()

os.chdir('..')

# if there is no results directory, create one
if not os.path.exists('results') :
	os.makedirs('results')

output = open('results/daily.html', 'w')
output.write('<!doctype html>' + "\n")
output.write('<html>' + "\n")
output.write(' <head>' + "\n")
output.write('  <meta charset="utf-8">' + "\n")
output.write('  <title>Analysis results</title>' + "\n")
output.write('  <link rel="stylesheet" href="analysis.css" />' + "\n")
output.write(' </head>' + "\n")
output.write(' <body>' + "\n")
output.write('  <h1>Analysis results on the Wikidata dump</h1>' + "\n")
output.write('  <p>' + "\n")
output.write('   As of: ' + str(lastdaily) + '<br>' + "\n")
output.write('   Pages: ' + str(len(processedpages)) + '<br>' + "\n")
output.write('   Items: ' + str(itemcount) + '<br>' + "\n")
output.write('   Items with claims: ' + str(itemswithclaims) + '<br>' + "\n")
output.write('   Claims: ' + str(claimcount) + '<br>' + "\n")
output.write('   Claims per item: ' + str(claimsperitem) + '<br>' + "\n")
output.write('   References: ' + str(refs) + '<br>' + "\n")
output.write('   Claims with references: ' + str(claimswithrefs) + '<br>' + "\n")
output.write('   Items with references: ' + str(itemswithrefs) + '<br>' + "\n")
output.write('   Item with most claims: ' + titleofmostclaims + '<br>' + "\n")
output.write('   Properties: ' + str(propertycount) + '<br>' + "\n")
output.write('   Links: ' + str(sitelinkcount) + '<br>' + "\n")
output.write('   Links per language: ' + str(langsitelinks) + '<br>' + "\n")
output.write('   Labels: ' + str(labelcount) + '<br>' + "\n")
output.write('   Labels per language: ' + str(langlabels) + '<br>' + "\n")
output.write('   Labels of properties: ' + str(propertylabelcount) + '<br>' + "\n")
output.write('   Descriptions: ' + str(descriptioncount) + '<br>' + "\n")
output.write('   Descriptions per language: ' + str(langdescriptions) + '<br>' + "\n")
output.write('   Descriptions of properties: ' + str(propertydescriptioncount) + '<br>' + "\n")
output.write('   Revisions: ' + str(revisioncount) + '<br>' + "\n")
output.write('   Item revisions: ' + str(itemrevisioncount) + '<br>' + "\n")
output.write('   Bot revisions: ' + str(botrevisioncount) + '<br>' + "\n")
output.write('   Lines: ' + str(linecount) + '<br>' + "\n")
output.write('   Characters: ' + str(charactercount) + '<br>' + "\n")
output.write('   Time: ' + str(time.time() - start_time) + ' seconds<br>' + "\n")
output.write('  </p>' + "\n")
output.write(' </body>' + "\n")
output.write('</html>' + "\n")
output.close()

log('Done.')
