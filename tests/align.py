import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        el = self.xpath('p[@align="left"]')
        self.assertEqual("Left", el.text)
        el = self.xpath('p[@align="right"]')
        self.assertEqual("Right", el.text)
