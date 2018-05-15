import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        b1 = self.xpath('span[1]/button')
        b2 = self.xpath('span[2]/button')
        for _ in range(3):
            b1.click()
        for _ in range(5):
            b2.click()
        self.assertEqual("3\n5", self.body_text())
