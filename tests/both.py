import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('Both/main')
        t = self.xpath('form/input[@type=\'text\']')
        t.send_keys('hello')
        l = self.xpath('form/input[@type=\'submit\']')
        l.click()
        self.assertEqual("", self.body_text())
