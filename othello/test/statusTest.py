'''
Created on Mar 30, 2020

@author: Ye Zhao
'''

from unittest import TestCase
from othello.status import _status as status

class StatusTest(TestCase):
    
    def setUp(self):
        self.nominalLight = 1
        self.nominalDark = 2
        self.nominalBlank = 3
        self.nominalBorad = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        self.integrity = '6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b'
        self.inputDictionary = {}
        self.errorValue = 'error:'
        self.errorKey = 'error'
        
    def TearDown(self):
        self.inputDictionary = {}
    
    def setLight(self, light):
        self.inputDictionary['light'] = light
    
    def setDark(self, dark):
        self.inputDictionary['dark'] = dark
        
    def setBlank(self, blank):
        self.inputDictionary['blank'] = blank
    
    def setBoard(self, board):
        self.inputDictionary['board'] = board
    
    def setIntegrity(self, integrity):
        self.inputDictionary['integrity'] = integrity
    
    def setExtra(self, extra):
        self.inputDictionary['extra'] = extra
    
    #100 status
    #   Desired level of confidence: status analysis
    #   Input-output Analysis
    #      inputs:  light -> integer, .GE.0 .LE.9, default to 1, mandatory, unvalidated
    #               dark -> integer, .GE.0 .LE.9, default to 2, mandatory, unvalidated
    #               blank -> integer, .GE.0 .LE.9, default to 0, mandatory, unvalidated
    #               board -> n*n grid, .GE.6 .LE.16, mandatory, unvalidated
    #               integrity -> 64-character sha256 hash hexdigest, mandatory, unvalidated
    #      outputs: dictionary key-value pair
    #   Happy path analysis:
    #      light:   nominal value    light=1
    #               high bound       light=9
    #               low bound        light=0
    #               missing light    light=default value 1
    #               default value    light=1
    #      dark:    nominal value    dark=2
    #               high bound       dark=9
    #               low bound        dark=0
    #               missing dark     dark=default value 2
    #               default value    dark=2
    #      blank:   nominal value    blank=3
    #               high bound       blank=9
    #               low bound        blank=0
    #               missing blank    blank=default value 0
    #               default value    blank=0
    #      board:   nominal value    board=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    #               low bound size board with nominal elements board=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    #               high bound size board with nominal elements board=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,2,1,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    #      integrity: dark next player  integrity=f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465a
    #                 light next player integrity=66271cbb9037c515e73be3a74a37259a179f2d2861cf4e82130cd579a2141093
    #      output:  
    #               nominal light, nominal dark, nominal blank, nominal board, nominal integrity
    #               high bound light, nominal dark, nominal blank, nominal size, nominal board, nominal integrity
    #               low bound light, nominal dark, nominal blank, nominal size, nominal board, nominal integrity
    #               missing light, nominal dark, nominal blank, nominal size, nominal board, nominal integrity
    #               nominal light, low bound dark, nominal blank, nominal size, nominal board, nominal integrity
    #               nominal light, high bound dark, nominal blank, nominal size, nominal board, nominal integrity
    #               nominal light, missing dark, nominal blank, nominal size, nominal board, nominal integrity
    #               nominal light, nominal dark, low bound blank, nominal size, nominal board, nominal integrity
    #               nominal light, nominal dark, high bound blank, nominal size, nominal board, nominal integrity
    #               nominal light, nominal dark, missing blank, nominal size, nominal board, nominal integrity
    #               nominal light, nominal dark, nominal blank, low bound size board with nominal elements, nominal integrity
    #               nominal light, nominal dark, nominal blank, high bound size board with nominal elements, nominal integrity
    #               nominal light, nominal dark, nominal blank, nominal board, dark next player
    #               nominal light, nominal dark, nominal blank, nominal board, light next player
    #               status is "ok"
    #               status is "dark"
    #               status is "light"
    #               status is "end"
    #   Sad path analysis
    #      light:   above bound   light=10
    #               below bound   light=-1
    #               non-integer   light=X
    #               null light    light=
    #      dark:    above bound   dark=10
    #               below bound   dark=-1
    #               non-integer   dark=w
    #               null dark    dark=
    #      blank:   above bound   blank=10
    #               below bound   blank=-1
    #               non-integer   blank=w
    #               null blank    blank=
    #      board:   non-square board
    #               odd * odd board
    #               missing board
    #               null board
    #      integrity:  short integrity
    #                  long integrity
    #                  non hex characters
    #                  missing integrity
    #                  null integrity
    #      output:    
    #                  above bound light, nominal dark, nominal blank, nominal board, nominal integrity
    #                  below bound light, nominal dark, nominal blank, nominal size
    #                  non-integer light, nominal dark, nominal blank, nominal size
    #                  null light, nominal dark, nominal blank, nominal size
    #                  nominal light, above bound dark, nominal blank, nominal size
    #                  nominal light, below bound dark, nominal blank, nominal size
    #                  nominal light, non-integer dark, nominal blank, nominal size
    #                  nominal light, null dark, nominal blank, nominal size
    #                  nominal light, nominal dark, above bound blank, nominal size
    #                  nominal light, nominal dark, below bound blank, nominal size
    #                  nominal light, nominal dark, non-integer blank, nominal size
    #                  nominal light, nominal dark, null blank, nominal size
    #                  nominal light, nominal dark, nominal blank, non-square board, nominal integrity
    #                  nominal light, nominal dark, nominal blank, odd x odd board, nominal integrity
    #                  nominal light, nominal dark, nominal blank, missing board, nominal integrity
    #                  nominal light, nominal dark, nominal blank, null board, nominal integrity
    #                  nominal light, nominal dark, nominal blank, nominal board, short integrity 
    #                  nominal light, nominal dark, nominal blank,  nominal board, long integrity
    #                  nominal light, nominal dark, nominal blank, noninal board, non hex characters
    #                  nominal light, nominal dark, nominal blank,  nominal board, missing integrity
    #                  nominal light, nominal dark, nominal blank,  nominal board, null integrity
    #                  nominal light, dark = light, nominal blank, nominal board, nominal integrity
    #                  nominal light, nominal dark, blank = light, nominal board, nominal integrity
    #                  nominal light, nominal dark, blank = dark, nominal board, nominal integrity
    #                  nominal light, nominal dark, nominal blank, board with non-light/dark/blank values, nominal integrity
    #                  nominal light, nominal dark, nominal blank, nominal board, invalid integrity
    #                  nominal light, nominal dark, nominal blank,  board with non-light/dark/blank tokens, nominal integrity
    
    #Happy Path
    
    def test100_010NominalValue(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_020HighBoundLight(self):
        self.setLight(9)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,2,0,0,0,0,2,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('5ab81cb67067273363db989119448a0b878896f7db5c268a50c4ae3062cb3647')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_021LowBoundLight(self):
        self.setLight(0)
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,2,1,1,1,1,2,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('1b7e612b959852acbaf6b55d3f6b8dab2cdc32248a58a89dcf022ae80e5b36de')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_022MissingLight(self):
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465a')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_030LowBoundDark(self):
        self.setLight(5)
        self.setDark(0)
        self.setBlank(9)
        self.setBoard([9,9,9,9,9,9,9,9,9,9,9,9,9,9,5,0,9,9,9,9,0,5,9,9,9,9,9,9,9,9,9,9,9,9,9,9])
        self.setIntegrity('85c972c79b667135f99ad9380f4af4a7495c5b5de3768c9cb36c4bc73f0da08a')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_031HighBoundDark(self):
        self.setLight(5)
        self.setDark(9)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,5,9,3,3,3,3,9,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('34932b7f4bbafed18cf99e367e29407e6aae8b49b2ced711f31e429e7efc2a12')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_032MissingDark(self):
        self.setLight(5)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,5,2,3,3,3,3,2,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('a348c2dae89e65378fc64d889b1d394819c021b2e4cccb37310bbef9335bb900')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_040LowBoundBlank(self):
        self.setLight(5)
        self.setDark(6)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,6,0,0,0,0,6,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('062f219e852404144cd7967bcbac5d5d82c151697d8eacfd8c29779acbc58b19')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_041HighBoundBlank(self):
        self.setLight(5)
        self.setDark(6)
        self.setBlank(9)
        self.setBoard([9,9,9,9,9,9,9,9,9,9,9,9,9,9,5,6,9,9,9,9,6,5,9,9,9,9,9,9,9,9,9,9,9,9,9,9])
        self.setIntegrity('5b698f38d9d1c1754df196ee688f3900ceba9d074cb74b5e17c19a197b69bf02')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_042MissingBlank(self):
        self.setLight(5)
        self.setDark(6)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,6,0,0,0,0,6,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('062f219e852404144cd7967bcbac5d5d82c151697d8eacfd8c29779acbc58b19')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_050LowBoundSizeBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_051HighBoundSizeBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,2,1,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('5df1fd1ccbd0dc74d65ab00d4d62f2e21c2def95dc47e7c73751986cdb5e8710')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_060DarkNextPlayer(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465a')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_061LightNextPlayer(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('66271cbb9037c515e73be3a74a37259a179f2d2861cf4e82130cd579a2141093')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_070StatusisOK(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'status': 'ok'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_071StatusisDark(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,0,1,1,1,1,0])
        self.setIntegrity('e2f7b8593ebadc126833074a7d8653d3c12c36ab3b7622a9cc6ac5dc1a0d9698')
        expectResult = {'status': 'dark'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_072StatusisLight(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3])
        self.setIntegrity('7c53df9ff782bbbff544d876f4d69a1d87d5864295c0e4a6bf29e6a7ee5a96fc')
        expectResult = {'status': 'light'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_073StatusisEnd(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0, 1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,2,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1])
        self.setIntegrity('8a1c0659575e8cdd01b2e4ff3f431c845e7e7960279bb7abfaa5465e4a755354')
        expectResult = {'status': 'end'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    #Sad Path
    
    def test_900AboveBoundLight(self):
        self.setLight(10)
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,10,2,1,1,1,1,2,10,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('b71bf3bee30fb8c3caa49752bcf9656870cfbd3bec4e4353e1e491054bf11c2f')
        expectResult = {'status': 'error: above bound light'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_901BelowBoundLight(self):
        self.setLight(-1)
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,2,1,1,1,1,2,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('f31631fdc7ba5ecd3096a306dbc7e43a9bc13fa781b91d83c36057f5050a51da')
        expectResult = {'status': 'error: below bound light'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
        
    def test_902NonIntegerLight(self):
        self.setLight('X')
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,'X',2,1,1,1,1,2,'X',1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('8959fc376b23af1520014ef3bef1eb4f924ec692bbbcd9f638245bf85fb0a6da')
        expectResult = {'status': 'error: non integer light'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_903NullLight(self):
        self.setLight('')
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,2,1,1,1,1,2,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('1cc0050055aa122edbb536cc63dfe515e6a55132a42a6c8fa41349ab6e572c6a')
        expectResult = {'status': 'error: null light'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_910AboveBoundDark(self):
        self.setLight(5)
        self.setDark(10)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,10,1,1,1,1,10,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('e8a244c301df58429d82070942fe05dff389162c0aeec8383e3c82863ae09c62')
        expectResult = {'status': 'error: above bound dark'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_911BelowBoundDark(self):
        self.setLight(5)
        self.setDark(-1)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,-1,1,1,1,1,-1,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('301e0f00c1b83b65adc1d4fd5e87aaf7f594aa20842ab1df86a6be2e144367db')
        expectResult = {'status': 'error: below bound dark'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult) 
    
    def test_912NonIntegerDark(self):
        self.setLight(5)
        self.setDark(1.2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1.2,1,1,1,1,1.2,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('e62a2ec6eb082391a6a5664b4f4dbd8130e43d6589267b19b831423bfcde4a9d')
        expectResult = {'status': 'error: non integer dark'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_913NullDark(self):
        self.setLight(5)
        self.setDark('')
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('5d5aeb4a45b57eecf69dcc304664fcf7a6f7c74c86ef9ede14da46ab2d9df242')
        expectResult = {'status': 'error: null dark'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
        
    def test_920AboveBoundBlank(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(10)
        self.setBoard([10,10,10,10,10,10,10,10,10,10,10,10,10,10,1,2,10,10,10,10,2,1,10,10,10,10,10,10,10,10,10,10,10,10,10,10])
        self.setIntegrity('530242aec98aa07d3c025b9101bd5b840527cd9b03302641da18c801d70c37e8')
        expectResult = {'status': 'error: above bound blank'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_921BelowBoundBlank(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(-1)
        self.setBoard([-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,-1,-1,-1,-1,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1])
        self.setIntegrity('2e226315d3fc18cf5771b45ae78bfe7be9510ee98b6e566e382f8a70861c8e7d')
        expectResult = {'status': 'error: below bound blank'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_922NonIntegerBlank(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank('1E5')
        self.setBoard([1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1,2,1E5,1E5,1E5,1E5,2,1,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5])
        self.setIntegrity('fe62b7f99befb02e21c50cc755a68ef80fb59d56224b02a1f2888e0830454773')
        expectResult = {'status': 'error: non integer blank'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_923NullBlank(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank('')
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'status': 'error: null blank'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_930NonSquareBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('9d43a04297202bccc81a13b6857179269c0fe33e5227c6569286d54d82493ba6')
        expectResult = {'status': 'error: non square board'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_931OddBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('1e3f8bb2d56c5b4483c9f3dccf7bc16d339534a98020e9a28383aaa219f3e64d')
        expectResult = {'status': 'error: odd board'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_932MissingBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setIntegrity('1e3f8bb2d56c5b4483c9f3dccf7bc16d339534a98020e9a28383aaa219f3e64d')
        expectResult = {'status': 'error: missing board'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_933NullBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard('')
        self.setIntegrity('1e3f8bb2d56c5b4483c9f3dccf7bc16d339534a98020e9a28383aaa219f3e64d')
        expectResult = {'status': 'error: null board'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_934AboveBoungdBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3])
        self.setIntegrity('dcb5af316d3a6546f66ed87d90c9ee1c067509b0fe9f347b43d1c999031683d3')
        expectResult = {'status': 'error: above board'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_935BelowBoungdBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3, 3, 3, 3, 3, 1, 2, 3, 3, 2, 1, 3, 3, 3, 3, 3])
        self.setIntegrity('4c5f1af4b0738e9d421d1321cec061f3f2fdaf658ff42328b9be7cace38248de')
        expectResult = {'status': 'error: below board'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_940ShortIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465')
        expectResult = {'status': 'error: short integrity'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_941LongIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465a00')
        expectResult = {'status': 'error: long integrity'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_942NonHexCharactersIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465$')
        expectResult = {'status': 'error: non hex characters integrity'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_943MissingIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        expectResult = {'status': 'error: missing integrity'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_944NullIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('')
        expectResult = {'status': 'error: null integrity'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_950DarkEqualToLight(self):
        self.setLight(2)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,0,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('e50f93033edd2b27fd1c54631a4b574e545df9e8c06e0b4f74ca94841a4ab6c4')
        expectResult = {'status': 'error: dark and light have the same value'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_951BliankEqualToLight(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setIntegrity('c725061d80e342070c231d2b987c476f92b8f3d9e5826c2223cff281562e8e2c')
        expectResult = {'status': 'error: blank and light have the same value'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_952BliankEqualToDark(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(2)
        self.setBoard([2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2])
        self.setIntegrity('4edfe0aad5d491d98b8103e4f8f899cd3cef690f6ec3602a16e5a0e0301e8bd6')
        expectResult = {'status': 'error: blank and dark have the same value'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_954InvalidIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setIntegrity('4d5aeb4a45b57eecf69dcc304664fcf7a6f7c74c86ef9ede14da46ab2d9df242')
        expectResult = {'status': 'error: invalid integrity'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_955BoardwithNonLightTokens(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setIntegrity('c9fd7c0049f79f33e45998064cd1fca01600dd5cdc55cb3bf33169cd07c1905a')
        expectResult = {'status': 'error: non light/dark/blank tokens board'}
        actualResult = status(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    