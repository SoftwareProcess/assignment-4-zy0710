import hashlib

def create(parmDictionary):
    ERROR_HEADER = 'error: '
    ERROR_KEY = 'error'
    DEFAULT_LIGHT = 1
    DEFAULT_DARK = 2
    DEFAULT_BLANK = 0
    DEFAULT_SIZE = 8
    resultDict = {}
    
    # Validate input parameter values
    try:
        # Validate light
        if(not('light' in parmDictionary)):
            parmDictionary['light'] = DEFAULT_LIGHT
        light = parmDictionary['light']
        if(light == None):
            resultDict['status'] = 'error: null light value'
            return resultDict
        try:
            lightNumeric = float(light)
            if(lightNumeric - int(lightNumeric) > 0):
                resultDict['status'] = 'error: non-integer light value'
                return resultDict
            light = int(light)
        except:
            resultDict['status'] = 'error: non-integer light value'
            return resultDict
        if(light < 0):
            resultDict['status'] = 'error: below bound light value'
            return resultDict
        if(light > 9):
            resultDict['status'] = 'error: above bound light value'
            return resultDict
        
        # Validate dark
        if(not('dark' in parmDictionary)):
            parmDictionary['dark'] = DEFAULT_DARK
        dark = parmDictionary['dark']
        if(dark == None):
            resultDict['status'] = 'error: null dark value'
            return resultDict
        try:
            darkNumeric = float(dark)
            if(darkNumeric - int(darkNumeric) > 0):
                resultDict['status'] = 'error: non-integer dark value'
                return resultDict
            dark = int(dark)
        except:
            resultDict['status'] = 'error: non-integer dark value'
            return resultDict
        if(dark < 0):
            resultDict['status'] = 'error: below bound dark value'
            return resultDict
        if(dark > 9):
            resultDict['status'] = 'error: above bound dark value'
            return resultDict
        
        
        # Validate blank
        if(not('blank' in parmDictionary)):
            parmDictionary['blank'] = DEFAULT_BLANK
        blank = parmDictionary['blank']
        if(blank == None):
            resultDict['status'] = 'error: null blank value'
            return resultDict
        try:
            blankNumeric = float(blank)
            if(blankNumeric - int(blankNumeric) > 0):
                resultDict['status'] = 'error: float blank value'
                return resultDict
            blank = int(blank)
        except:
            resultDict['status'] = 'error: non-integer blank value'
            return resultDict
        if(blank < 0):
            resultDict['status'] = 'error: below bound blank value'
            return resultDict
        if(blank > 9):
            resultDict['status'] = 'error: above bound blank value'
            return resultDict
        
        # Validate Size
        if(not('size' in parmDictionary)):
            parmDictionary['size'] = DEFAULT_SIZE
        size = parmDictionary['size']
        if(size == None):
            resultDict['status'] = 'error: null size value'
            return resultDict  
        try:
            sizeNumeric = float(size)
            if(sizeNumeric - int(sizeNumeric) > 0):
                resultDict['status'] = 'error: non-integer size value'
                return resultDict  
            size = int(size)
        except:
            resultDict['status'] = 'error: non-integer size value'
            return resultDict
        if(size < 6):
            resultDict['status'] = 'error: below bound size value'
            return resultDict
        if(size > 16):
            resultDict['status'] = 'error: above bound size value'
            return resultDict
        
        halfsize = size / 2
        halfsizeNumeric = float(halfsize)
        if(halfsizeNumeric - int(halfsizeNumeric) > 0):
            resultDict['status'] = 'error: odd size value'
            return resultDict
        size = int(size)
        
        if(dark == light):
            resultDict['status'] = 'error: light and dark have the same value'
            return resultDict
        if(blank == light):
            resultDict['status'] = 'error: light and blank have the same value'
            return resultDict
        if(dark == blank):
            resultDict['status'] = 'error: dark and blank have the same value'
            return resultDict
            
    
    # Catch validation problems and return error diagnostic
    except Exception as e:
        result = ERROR_HEADER + e.args[0]
        resultDict[ERROR_KEY] = result
        return resultDict  
        
    # validate the board
    boardlist = [[] for i in range (size)]
    for i in range (0,size):
        for j in range(0,size):
            boardlist[i].append(blank)
            j = j+1
    index1 = size // 2 - 1
    index2 = size // 2 + 1 -1
    boardlist[index1][index1] = light
    boardlist[index2][index2] = light
    boardlist[index1][index2] = dark
    boardlist[index2][index1] = dark
    
    finalboard = []
    for a in range(0,size):
        for b in range(0,size):
            finalboard.append(boardlist[a][b])
            b = b+1
        a = a+1
    
    resultDict['board'] = finalboard
    
    # validate the tokens
    tokensdict = dict()
    tokensdict['light'] = light
    tokensdict['dark'] = dark
    tokensdict['blank'] = blank
    
    resultDict['tokens'] = tokensdict
    
    # validate the integrity
    finalboard_new = [str(x) for x in finalboard]
    strboard = ''.join(finalboard_new)
    
    next_player = dark
    followinglist = []
    followinglist.append('/')
    followinglist.append(light)
    followinglist.append('/')
    followinglist.append(dark)
    followinglist.append('/')
    followinglist.append(blank)
    followinglist.append('/')
    followinglist.append(next_player)
    followinglist_new = [str(x) for x in followinglist]
    strfollowinglist = ''.join(followinglist_new)
    
    str_new = strboard + strfollowinglist
    
    h = hashlib.sha256()
    h.update(str_new.encode())
    integrity = h.hexdigest()
    
    resultDict['integrity'] = integrity
    
    resultDict['status'] = 'ok'
    return resultDict

    
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
