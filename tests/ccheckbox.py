import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('main')
        d = self.xpath('input')
        p = self.xpath('span')
        self.assertEqual("True 1", p.text)
        d.click()
        # the elements gets re-created from scratch
        # so we must refresh our reference
        p = self.xpath('span')
        self.assertEqual("False 3", p.text)
