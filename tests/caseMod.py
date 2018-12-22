import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('main')
        l1 = self.xpath('li[1]/a')
        l1.click()

        self.assertEqual("C A\n\nAgain!", self.body_text())
    def test_2(self):
        """Test case 2"""
        self.start('main')
        l1 = self.xpath('li[2]/a')
        l1.click()

        self.assertEqual("C B\n\nAgain!", self.body_text())
    def test_3(self):
        """Test case 3"""
        self.start('main')
        l1 = self.xpath('li[3]/a')
        l1.click()

        self.assertEqual("D\n\nAgain!", self.body_text())
