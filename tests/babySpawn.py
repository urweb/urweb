import unittest
import base
import time 

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        btn = self.xpath('button')
        btn.click()
        alert = self.driver.switch_to.alert
        self.assertEqual("Hi", alert.text)
