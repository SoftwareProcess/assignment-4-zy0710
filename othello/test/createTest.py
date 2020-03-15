from unittest import TestCase
from othello.create import create as create

import json

class CreateTest(TestCase):
    
    def setUp(self):
        self.nominalLight = 6
        self.nominalDark = 5
        self.nominalBlank = 1
        self.nominalSize = 10
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
    
    def setSize(self, size):
        self.inputDictionary['size'] = size
    
    def setExtra(self, extra):
        self.inputDictionary['extra'] = extra
    
    #100 create
    #   Desired level of confidence: initial board value analysis
    #   Input-output Analysis
    #      inputs:      light -> integer, .GE.0 .LE.9, default to 1, mandatory, unvalidated
    #                   dark -> integer, .GE.0 .LE.9, default to 2, mandatory, unvalidated
    #                   blank -> integer, .GE.0 .LE.9, default to 0, mandatory, unvalidated
    #                   size -> integer, .GE.6 .LE.16, default to 8, mandatory, unvalidated
    #      outputs:     dictionary value
    #   Happy path analysis:
    #      light:   nominal value    light=6
    #               high bound       light=9
    #               low bound        light=0
    #               missing light    light=default value 1
    #               default value    light=1
    #      dark:    nominal value    dark=5
    #               high bound       dark=9
    #               low bound        dark=0
    #               missing dark     dark=default value 2
    #               default value    dark=2
    #      blank:   nominal value    blank=1
    #               high bound       blank=9
    #               low bound        blank=0
    #               missing blank    blank=default value 0
    #               default value    blank=0
    #      size:    nominal value    size=10
    #               high bound       size=16
    #               low bound        size=6
    #               missing size     size=default value 8
    #               default value    size=8
    #      output:
    #               nominal light, nominal dark, nominal blank, nominal size
    #               high bound light, nominal dark, nominal blank, nominal size
    #               low bound light, nominal dark, nominal blank, nominal size
    #               missing light, nominal dark, nominal blank, nominal size
    #               nominal light, high bound dark, nominal blank, nominal size
    #               nominal light, low bound dark, nominal blank, nominal size
    #               nominal light, missing dark, nominal blank, nominal size
    #               nominal light, nominal dark, high bound blank, nominal size
    #               nominal light, nominal dark, low bound blank, nominal size
    #               nominal light, nominal dark, missing blank, nominal size
    #               nominal light, nominal dark, nominal blank, high bound size
    #               nominal light, nominal dark, nominal blank, low bound size
    #               nominal light, nominal dark, nominal blank, missing size
    #               all parameters are defaulted
    #               extraneous parameters are ignored
    #   Sad path analysis
    #      light:   above bound   light=10
    #               below bound   light=-1
    #               non-integer   light=w
    #               null light    light=
    #      dark:    above bound   dark=10
    #               below bound   dark=-1
    #               non-integer   dark=w
    #               null dark    dark=
    #      blank:   above bound   blank=10
    #               below bound   blank=-1
    #               non-integer   blank=w
    #               null blank    blank=
    #      size:    above bound   size=17
    #               below bound   size=5
    #               non-integer   size=1.2
    #               null blank    size=
    #      output:
    #               above bound light, nominal dark, nominal blank, nominal size
    #               below bound light, nominal dark, nominal blank, nominal size
    #               non-integer light, nominal dark, nominal blank, nominal size
    #               null light, nominal dark, nominal blank, nominal size
    #               nominal light, above bound dark, nominal blank, nominal size
    #               nominal light, below bound dark, nominal blank, nominal size
    #               nominal light, non-integer dark, nominal blank, nominal size
    #               nominal light, null dark, nominal blank, nominal size
    #               nominal light, nominal dark, above bound blank, nominal size
    #               nominal light, nominal dark, below bound blank, nominal size
    #               nominal light, nominal dark, non-integer blank, nominal size
    #               nominal light, nominal dark, null blank, nominal size
    #               nominal light, nominal dark, nominal blank, above bound size
    #               nominal light, nominal dark, nominal blank, below bound size
    #               nominal light, nominal dark, nominal blank, non-integer size
    #               nominal light, nominal dark, nominal blank, null size
    #               nominal light, dark=light, nominal blank, nominal size
    #               nominal light, nominal dark, blank= light, nominal size
    #               nominal light, nominal dark, blank = dark, nominal size
    
    
    #Happy path
    
    def test100_010NominalValue(self):
        self.setLight(6)
        self.setDark(5)
        self.setBlank(1)
        self.setSize(10)
        expectResult = {'board': [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 6, 5, 1, 1, 1, 1, 1, 1, 1, 1, 5, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], 'tokens': {'light': 6, 'dark': 5, 'blank': 1}, 'status': 'ok', 'integrity': 'd0f18c5b412ab1dbf89da19baa33cc35f4a7dd0619ce7b7dcb2381d2cb14a412'}
        actualResult = create(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_020HighBoundLight(self):
        self.setLight(9)
        self.setDark(5)
        self.setBlank(1)
        self.setSize(10)
        expectResult = {'board': [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 5, 1, 1, 1, 1, 1, 1, 1, 1, 5, 9, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], 'tokens': {'light': 9, 'dark': 5, 'blank': 1}, 'status': 'ok', 'integrity': '723c769319c6529cf8520336232a9e5d281be77df1455c6ceb10a5d1d4733236'}
        actualResult = create(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_030LowBoundLight(self):
        self.setLight(0)
        self.setDark(5)
        self.setBlank(1)
        self.setSize(10)
        expectResult = {'board': [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 5, 1, 1, 1, 1, 1, 1, 1, 1, 5, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1], 'tokens': {'light': 0, 'dark': 5, 'blank': 1}, 'status': 'ok', 'integrity': '4bd2efa7e0d5f13551f7277950e45b6fcfe7d5159b80823a5dcbdf57abb4d83a'}
        actualResult = create(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    