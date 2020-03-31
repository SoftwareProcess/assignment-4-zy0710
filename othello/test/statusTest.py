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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
        
        