import json
from collections import defaultdict

alllangs = set([u'gv', u'gu', u'scn', u'km', u'wuu', u'cdo', u'sco', u'kbd', u'gd', u'fiu_vro', u'ga', u'gn', u'gl', u'als', u'lg', u'pnb', u'lb', u'szl', u'vep', u'la', u'ln', u'frp', u'tt', u'tr', u'ts', u'li', u'lv', u'bat_smg', u'lt', u'vec', u'th', u'ti', u'tg', u'te', u'ksh', u'pcd', u'ta', u'yi', u'ceb', u'yo', u'de', u'da', u'bxr', u'dz', u'hif', u'rm', u'dv', u'be_x_old', u'nov', u'kaa', u'bar', u'ang', u'fur', u'kn', u'eml', u'bpy', u'crh', u'nds_nl', u'diq', u'ho', u'el', u'eo', u'en', u'zh', u'pms', u'ee', u'tpi', u'arz', u'rmy', u'mdf', u'za', u'mh', u'arc', u'uk', u'eu', u'et', u'tet', u'es', u'ru', u'rw', u'mus', u'kl', u'ha', u'ak', u'bm', u'new', u'rn', u'ro', u'dsb', u'bn', u'hsb', u'be', u'bg', u'myv', u'ba', u'wa', u'ast', u'wo', u'got', u'jv', u'bo', u'bh', u'bi', u'rue', u'hak', u'tum', u'br', u'bs', u'lez', u'ja', u'om', u'glk', u'ace', u'ng', u'ilo', u'ty', u'oc', u'ltg', u'st', u'lo', u'krc', u'nds', u'os', u'xmf', u'udm', u'xh', u'ch', u'co', u'nso', u'simple', u'bjn', u'ca', u'lmo', u'ce', u'cy', u'ab', u'cs', u'cr', u'cv', u'cu', u've', u'koi', u'ps', u'fj', u'or', u'srn', u'pt', u'to', u'sm', u'ext', u'tl', u'cho', u'frr', u'chr', u'pa', u'xal', u'chy', u'pi', u'war', u'pl', u'tk', u'hz', u'hy', u'nrm', u'hr', u'iu', u'pfl', u'ht', u'hu', u'gan', u'hi', u'vls', u'tw', u'gag', u'an', u'bug', u'kj', u'he', u'mg', u'roa_tara', u'uz', u'ml', u'mo', u'mn', u'mi', u'as', u'mk', u'ur', u'zea', u'mt', u'stq', u'ms', u'mr', u'zh_classical', u'ku', u'ug', u'mwl', u'my', u'ki', u'pih', u'mhr', u'aa', u'sah', u'ss', u'af', u'tn', u'vi', u'is', u'am', u'it', u'vo', u'ii', u'ay', u'ik', u'ar', u'lbe', u'zh_yue', u'io', u'av', u'ia', u'haw', u'az', u'ie', u'id', u'ig', u'pap', u'qu', u'nl', u'nn', u'no', u'na', u'nah', u'ne', u'lij', u'csb', u'ny', u'nap', u'zh_min_nan', u'cbk_zam', u'map_bms', u'pag', u'zu', u'so', u'pam', u'nv', u'kv', u'kab', u'fr', u'roa_rup', u'lad', u'fy', u'pnt', u'fa', u'ks', u'ff', u'fi', u'mzn', u'ky', u'fo', u'bcl', u'ka', u'kg', u'ckb', u'kk', u'sr', u'sq', u'min', u'ko', u'sv', u'su', u'jbo', u'sk', u'kr', u'si', u'sh', u'kw', u'sn', u'mrj', u'sl', u'sc', u'sa', u'sd', u'sg', u'sw', u'se', u'pdc'])
#to keep order on alllangs
alllangs = list(alllangs)                                                                                                                                                                       
alllangs.sort()


langsofitemjson = open('langsofitem.json', 'r')
langsofitem = json.load(langsofitemjson)
                                                                                                                                                                                                                                                                                                                                                                                                                                        
#our main datastructure
intersections = defaultdict(int)
numintersections = defaultdict(int)
langtotals = defaultdict(int)

def makeKey(langlist):
    returnkey = ''
    for lang in alllangs:
        if lang in langlist:
            returnkey += '1'
        else:
            returnkey += '0'
    return returnkey

def numOfLangs(binkey):
    #this is the digitsum
        return sum(map(int, binkey))
        

def reverseKey(langsofitem):
    for item, langlist in langsofitem.iteritems():
        itemKey = makeKey(langlist)
        intersections[itemKey] += 1
    #print intersections          
    #print len(intersections)

def reverseKeyFilterN(langsofitem, n):
    total, ngrams = 0, 0
    for item, langlist in langsofitem.iteritems():
        total += 1
        #update lang totals
        for lang in langlist:
            langtotals[lang] += 1
        #record the ones of lenght n
        if len(langlist) == n:
            ngrams += 1
            langlist.sort()
            langlistkey = reduce(lambda x,y: x+'*'+y,langlist)
            intersections[langlistkey] += 1
    print len(intersections)
    
    if n == 1:
        for lang in alllangs:
            if not lang in intersections:
                print lang
                intersections[lang] += 0
                
    numintersections[n] = ngrams
    print 'total', total, 'ngrams', ngrams
    
def exportToJSON(intersections):
    filternjson = open('filtern.json', 'w')
    json.dump(obj=intersections, fp=filternjson)
    #langtotalsjson = open('langtotals.json', 'w')
    #json.dump(obj=langtotals, fp=langtotalsjson)

def exportToR(intersections):
    csvfile = open('langsofitemR.csv', 'w')
    line = ''
    #write headers
    for lang in alllangs:
        if line == '':
            line += lang
        else: 
            line += (', ' + lang)
    line += '\n'
    csvfile.write(line)
    line = ''
    for binaryRepresentation, count in intersections.iteritems():
        for binary in binaryRepresentation:
            line += (binary + ',')
        line += (count + '\n')  
    

    
#reverseKey(langsofitem)
#reverseKeyFilterN(langsofitem, 2)
'''
for i in range(1, len(alllangs)):
    print i
    reverseKeyFilterN(langsofitem, i)
exportToJSON(numintersections)
'''
reverseKeyFilterN(langsofitem, 3)
exportToJSON(intersections)
#exportToR(intersections)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    