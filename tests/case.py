import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('main')
        d = self.xpath('div')
        txt = "zero is two: B\none is two: B\ntwo is two: A"
        self.assertEqual(txt, d.text)

        b = self.xpath('button')
        b.click()
        alert = self.driver.switch_to.alert
        self.assertEqual(txt, alert.text)
