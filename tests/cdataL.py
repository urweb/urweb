import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('main')
        l1 = self.xpath('li[1]/a')
        l1.click()

        self.assertEqual("<Hi.", self.body_text())
    def test_2(self):
        """Test case 2"""
        self.start('main')
        l1 = self.xpath('li[2]/a')
        l1.click()

        self.assertEqual("Bye.", self.body_text())
