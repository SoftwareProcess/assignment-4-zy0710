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
    
    def test100_040LowBoundBlank(self):
        self.setLight(5)
        self.setDark(6)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,6,0,0,0,0,6,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('2:3')
        self.setIntegrity('062f219e852404144cd7967bcbac5d5d82c151697d8eacfd8c29779acbc58b19')
        expectResult = {'board':[0,0,0,0,0,0,0,0,6,0,0,0,0,0,6,6,0,0,0,0,6,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 'integrity': 'd599b0771ef0a0ce11d0b974fde35cc863896da7161d35c7fd9a34a342f837d6', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
        
    def test100_041HighBoundBlank(self):
        self.setLight(5)
        self.setDark(6)
        self.setBlank(9)
        self.setBoard([9,9,9,9,9,9,9,9,9,9,9,9,9,9,5,6,9,9,9,9,6,5,9,9,9,9,9,9,9,9,9,9,9,9,9,9])
        self.setLocation('2:3')
        self.setIntegrity('5b698f38d9d1c1754df196ee688f3900ceba9d074cb74b5e17c19a197b69bf02')
        expectResult = {'board':[9,9,9,9,9,9,9,9,6,9,9,9,9,9,6,6,9,9,9,9,6,5,9,9,9,9,9,9,9,9,9,9,9,9,9,9], 'integrity': 'c3127c5de31120bfcde5ae650619a9625cfa71799ddfaff375cf198dc7151b78', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_042MissingBlank(self):
        self.setLight(5)
        self.setDark(6)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,6,0,0,0,0,6,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('2:3')
        self.setIntegrity('062f219e852404144cd7967bcbac5d5d82c151697d8eacfd8c29779acbc58b19')
        expectResult = {'board':[0,0,0,0,0,0,0,0,6,0,0,0,0,0,6,6,0,0,0,0,6,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 'integrity': 'd599b0771ef0a0ce11d0b974fde35cc863896da7161d35c7fd9a34a342f837d6', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_050LowBoundSizeBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('2:3')
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'board':[0,0,0,0,0,0,0,0,2,0,0,0,0,0,2,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 'integrity': 'eaf8d3a826f7f59529add5f9eb60310ab9e936b3556e64a35ac67fef8370094a', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_051HighBoundSizeBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,1,2,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,2,1,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('7:8')
        self.setIntegrity('5df1fd1ccbd0dc74d65ab00d4d62f2e21c2def95dc47e7c73751986cdb5e8710')
        expectResult = {'board':[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,2,2,0,0,0,0,0,0,0,   0,0,0,0,0,0,0,2,1,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 'integrity': '22c9a07918ebc2514da274d5be19200fab4bdcc2fc2528607e28d574701d7c3e', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_060DarkNextPlayer(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465a')
        expectResult = {'board':[3,3,3,3,3,3,3,3,2,3,3,3,3,3,2,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3], 'integrity': 'b98faa3e819e6243f52dfc23f0829b0c75b1675d73c75148c530e86e7c670fe5', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_061LightNextPlayer(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('3:5')
        self.setIntegrity('66271cbb9037c515e73be3a74a37259a179f2d2861cf4e82130cd579a2141093')
        expectResult = {'board':[3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,1,1,3,3,3,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3], 'integrity': '483f069517e1567f42c89763c64b16095150048be47cd1c1013087be596d0421', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_070StatusisDark(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,2,2,2,2,2,2,0,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,0,1,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2])
        self.setLocation('1:1')
        self.setIntegrity('45e867b1afe42012deea7c58e0826a4a6935caf63a20b85b9869f84471137ceb')
        expectResult = {'board':[2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,0,1,2,2,2,2,2,2,2,0,2,2,2,2,2,2,2,2], 'integrity': '8bb12cf2c0dbe997bc47bf165e6e1ad84c90ed0e818e47eb3e1e35097c5d5ca7', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_071StatusisLight(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,1,1,1,1,1,1,0,2,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,2,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1])
        self.setLocation('1:1')
        self.setIntegrity('1aab07232eb70d3c2b06707b22fd56b00db11919b8e63d46f63f7fdf2ff21edf')
        expectResult = {'board':[1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,0,2,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1], 'integrity': 'ab0e84170348151b2fb263e2b490cede3ef4154f7ad747456c697f3e7b554679', 'status': 'ok'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test100_072StatusisEnd(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([1,1,1,1,1,1,1,0, 1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0, 1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,2,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1])
        self.setLocation('1:8')
        self.setIntegrity('5adb44539951fb3754715c52f2f4759490314d97c02e04f13282356809e3cafc')
        expectResult = {'board':[1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0, 1,1,1,1,1,1,0,0,1,1,1,1,1,1,0,2,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1], 'integrity': '8a1c0659575e8cdd01b2e4ff3f431c845e7e7960279bb7abfaa5465e4a755354', 'status': 'end:58/1'}        
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    #Sad Path
    
    def test_900AboveBoundLight(self):
        self.setLight(10)
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,10,2,1,1,1,1,2,10,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('b71bf3bee30fb8c3caa49752bcf9656870cfbd3bec4e4353e1e491054bf11c2f')
        expectResult = {'status': 'error: above bound light'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_901BelowBoundLight(self):
        self.setLight(-1)
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,-1,2,1,1,1,1,2,-1,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('f31631fdc7ba5ecd3096a306dbc7e43a9bc13fa781b91d83c36057f5050a51da')
        expectResult = {'status': 'error: below bound light'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_902NonIntegerLight(self):
        self.setLight('X')
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,'X',2,1,1,1,1,2,'X',1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('8959fc376b23af1520014ef3bef1eb4f924ec692bbbcd9f638245bf85fb0a6da')
        expectResult = {'status': 'error: non integer light'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_903NullLight(self):
        self.setLight('')
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,3,2,1,1,1,1,2,3,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('1cc0050055aa122edbb536cc63dfe515e6a55132a42a6c8fa41349ab6e572c6a')
        expectResult = {'status': 'error: null light'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_910AboveBoundDark(self):
        self.setLight(5)
        self.setDark(10)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,10,1,1,1,1,10,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('e8a244c301df58429d82070942fe05dff389162c0aeec8383e3c82863ae09c62')
        expectResult = {'status': 'error: above bound dark'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_911BelowBoundDark(self):
        self.setLight(5)
        self.setDark(-1)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,-1,1,1,1,1,-1,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('301e0f00c1b83b65adc1d4fd5e87aaf7f594aa20842ab1df86a6be2e144367db')
        expectResult = {'status': 'error: below bound dark'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult) 
    
    def test_912NonIntegerDark(self):
        self.setLight(5)
        self.setDark(1.2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1.2,1,1,1,1,1.2,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('e62a2ec6eb082391a6a5664b4f4dbd8130e43d6589267b19b831423bfcde4a9d')
        expectResult = {'status': 'error: non integer dark'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
        
    def test_913NullDark(self):
        self.setLight(5)
        self.setDark('')
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('5d5aeb4a45b57eecf69dcc304664fcf7a6f7c74c86ef9ede14da46ab2d9df242')
        expectResult = {'status': 'error: null dark'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_920AboveBoundBlank(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(10)
        self.setBoard([10,10,10,10,10,10,10,10,10,10,10,10,10,10,1,2,10,10,10,10,2,1,10,10,10,10,10,10,10,10,10,10,10,10,10,10])
        self.setLocation('2:3')
        self.setIntegrity('530242aec98aa07d3c025b9101bd5b840527cd9b03302641da18c801d70c37e8')
        expectResult = {'status': 'error: above bound blank'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_921BelowBoundBlank(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(-1)
        self.setBoard([-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,1,2,-1,-1,-1,-1,2,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1])
        self.setLocation('2:3')
        self.setIntegrity('2e226315d3fc18cf5771b45ae78bfe7be9510ee98b6e566e382f8a70861c8e7d')
        expectResult = {'status': 'error: below bound blank'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_922NonIntegerBlank(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank('1E5')
        self.setBoard([1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1,2,1E5,1E5,1E5,1E5,2,1,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5,1E5])
        self.setLocation('2:3')
        self.setIntegrity('fe62b7f99befb02e21c50cc755a68ef80fb59d56224b02a1f2888e0830454773')
        expectResult = {'status': 'error: non integer blank'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_923NullBlank(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank('')
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('2:3')
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'status': 'error: null blank'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_930NonSquareBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('9d43a04297202bccc81a13b6857179269c0fe33e5227c6569286d54d82493ba6')
        expectResult = {'status': 'error: non square board'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_931OddBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('1e3f8bb2d56c5b4483c9f3dccf7bc16d339534a98020e9a28383aaa219f3e64d')
        expectResult = {'status': 'error: odd board'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_932MissingBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setLocation('2:3')
        self.setIntegrity('1e3f8bb2d56c5b4483c9f3dccf7bc16d339534a98020e9a28383aaa219f3e64d')
        expectResult = {'status': 'error: missing board'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_933NullBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard('')
        self.setLocation('2:3')
        self.setIntegrity('1e3f8bb2d56c5b4483c9f3dccf7bc16d339534a98020e9a28383aaa219f3e64d')
        expectResult = {'status': 'error: null board'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_934AboveBoungdBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 1, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3])
        self.setLocation('2:3')
        self.setIntegrity('dcb5af316d3a6546f66ed87d90c9ee1c067509b0fe9f347b43d1c999031683d3')
        expectResult = {'status': 'error: above board'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_935BelowBoungdBoard(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3, 3, 3, 3, 3, 1, 2, 3, 3, 2, 1, 3, 3, 3, 3, 3])
        self.setLocation('2:3')
        self.setIntegrity('4c5f1af4b0738e9d421d1321cec061f3f2fdaf658ff42328b9be7cace38248de')
        expectResult = {'status': 'error: below board'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_940ShortIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465')
        expectResult = {'status': 'error: short integrity'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_941LongIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465a00')
        expectResult = {'status': 'error: long integrity'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_942NonHexCharactersIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('f01977c17f801c43eeb13fb9f74a49bd0c761db3cdffe01510f47ddd23ab465$')
        expectResult = {'status': 'error: non hex characters integrity'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_943MissingIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        expectResult = {'status': 'error: missing integrity'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_944NullIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('')
        expectResult = {'status': 'error: null integrity'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_950UnplacableLocation(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('1:1')
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'status': 'error: cannot place location'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_951OutofBoundLocation(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('7:1')
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'status': 'error: out of bound location'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_960DarkEqualToLight(self):
        self.setLight(2)
        self.setDark(2)
        self.setBlank(0)
        self.setBoard([0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,2,0,0,0,0,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0])
        self.setLocation('2:3')
        self.setIntegrity('e50f93033edd2b27fd1c54631a4b574e545df9e8c06e0b4f74ca94841a4ab6c4')
        expectResult = {'status': 'error: dark and light have the same value'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_961BliankEqualToLight(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(1)
        self.setBoard([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1])
        self.setLocation('2:3')
        self.setIntegrity('c725061d80e342070c231d2b987c476f92b8f3d9e5826c2223cff281562e8e2c')
        expectResult = {'status': 'error: blank and light have the same value'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_962BliankEqualToDark(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(2)
        self.setBoard([2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,2,2,2,2,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2])
        self.setLocation('2:3')
        self.setIntegrity('4edfe0aad5d491d98b8103e4f8f899cd3cef690f6ec3602a16e5a0e0301e8bd6')
        expectResult = {'status': 'error: blank and dark have the same value'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    def test_963InvalidIntegrity(self):
        self.setLight(1)
        self.setDark(2)
        self.setBlank(3)
        self.setBoard([3,3,3,3,3,3,3,3,3,3,3,3,3,3,1,2,3,3,3,3,2,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3])
        self.setLocation('2:3')
        self.setIntegrity('4d5aeb4a45b57eecf69dcc304664fcf7a6f7c74c86ef9ede14da46ab2d9df242')
        expectResult = {'status': 'error: invalid integrity'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    