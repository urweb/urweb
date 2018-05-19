import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        txt = self.xpath('div')
        self.assertEqual('/', txt.text)
        inp = self.xpath('/input')
        inp.send_keys('hello there')
        self.assertEqual('hello there /', txt.text)
        btn = self.xpath('button')
        btn.click()
        self.assertEqual("hello there / hello there", txt.text)
