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
        self.nominallocation_row = 2
        self.nominallocation_column = 3
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
        
    def setLocation_row(self,location_row):
        self.inputDictionary['location_row'] = location_row
        
    def setLocation_column(self,location_column):
        self.inputDictionary['location_column'] = location_column
    
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
        self.setLocation_row(2)
        self.setLocation_column(3)
        self.setIntegrity('6c3ec0129f5e128f48e2541bd6663a52a825c35f99b9a69d9593f2fc44b0bb4b')
        expectResult = {'board': [0,0,0,0,0,0,0,0,2,0,0,0,0,0,2,2,0,0,0,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0], 'integrity': 'eaf8d3a826f7f59529add5f9eb60310ab9e936b3556e64a35ac67fef8370094a', 'status': 'ok'}
        actualResult = place(self.inputDictionary)
        self.assertEqual(expectResult,actualResult)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    