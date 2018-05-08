import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        el = self.xpath('form/input')
        val = el.get_attribute('value')
        self.assertEqual("\"Well hey\"\nWow", val)
