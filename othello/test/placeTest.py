'''
Created on Apr 27, 2020

@author: Ye Zhao
'''

from unittest import TestCase
from othello.place import _place as place

class StatusTest(TestCase):
    
    def setUp(self):
        self.nominalLight = 1
        self.nominalDark = 2
        self.nominalBlank = 3
        self.nominalBorad = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
        self.nominallocation = '2:3'
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
    
    def setLocation(self,location):
        self.inputDictionary['location'] = location
    
    def setIntegrity(self, integrity):
        self.inputDictionary['integrity'] = integrity
    
    def setExtra(self, extra):
        self.inputDictionary['extra'] = extra
    
    #Happy path test
    
    def test100_010NominalValue(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('2:3')
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'board': [0,0,0,0,0,0,0,0,2,0,0,0,0,0,2,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 'integrity': 'eaf8d3a826f7f59529add5f9eb60310ab9e936b3556e64a35ac67fef8370094a', 'status': 'ok'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_020HighBoundLight(self):
        self.setLight(9)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,2,0,0,0,0,2,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('2:3')
        self.setIntegrity('5ab81cb67067273363db989119448a0b878896f7db5c268a50c4ae3062cb3647')
        expectResult = {'board': [0,0,0,0,0,0,0,0,2,0,0,0,0,0,2,2,0,0,0,0,2,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 'integrity': 'f7b147bcf7a4639198e34d2f9c82b9d9524178142f53152c38ad421aa24bcb70', 'status': 'ok'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_021LowBoundLight(self):
        self.setLight(0)
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,2,1,1,1,1,2,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('1b7e612b959852acbaf6b55d3f6b8dab2cdc32248a58a89dcf022ae80e5b36de')
        expectResult = {'board': [1,1,1,1,1,1,1,1,2,1,1,1,1,1,2,2,1,1,1,1,2,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1], 'integrity': '9c0f1fb8ae907c1bdd92009367a2e2883a6e403edbdb379d47afe46939d26108', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_022MissingLight(self):
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465a')
        expectResult = {'board': [3,3,3,3,3,3,3,3,2,3,3,3,3,3,2,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3], 'integrity': 'b98faa3e819e6243f52dfc23f0829b0c75b1675d73c75148c530e86e7c670fe5', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_030LowBoundDark(self):
        self.setLight(5)
        self.setDark(0)
        self.setBlank(9)
        self.setBoard([9,9,9,9,9,9,9,9,9,9,9,9,9,9,5,0,9,9,9,9,0,5,9,9,9,9,9,9,9,9,9,9,9,9,9,9])
        self.setLocation('2:3')
        self.setIntegrity('85c972c79b667135f99ad9380f4af4a7495c5b5de3768c9cb36c4bc73f0da08a')
        expectResult = {'board': [9,9,9,9,9,9,9,9,0,9,9,9,9,9,0,0,9,9,9,9,0,5,9,9,9,9,9,9,9,9,9,9,9,9,9,9], 'integrity': '421b1e126d4c55423c2bc7277e49990cf7421c997d4a5c73dc2fc94085d2e056', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_031HighBoundDark(self):
        self.setLight(5)
        self.setDark(9)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,5,9,3,3,3,3,9,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('34932b7f4bbafed18cf99e367e29407e6aae8b49b2ced711f31e429e7efc2a12')
        expectResult = {'board': [3,3,3,3,3,3,3,3,9,3,3,3,3,3,9,9,3,3,3,3,9,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3], 'integrity': '69b5d68b6ad51e0db026a37f37b5579cd71a3665ee244a86600d2296c24ef929', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_032MissingDark(self):
        self.setLight(5)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,5,2,3,3,3,3,2,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('a348c2dae89e65378fc64d889b1d394819c021b2e4cccb37310bbef9335bb900')
        expectResult = {'board': [3,3,3,3,3,3,3,3,2,3,3,3,3,3,2,2,3,3,3,3,2,5,3,3,3,3,3,3,3,3,3,3,3,3,3,3], 'integrity': 'a8bd9fad80b5c3965e1867d4f85c964ec9c1014ecfecfd5d83b32618b7b5f57d', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
        
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    