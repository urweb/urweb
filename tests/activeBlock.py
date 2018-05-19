import unittest
import base
import time 

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        alert = self.driver.switch_to.alert
        self.assertEqual("Error: May not 'sleep' in main thread of 'code' for <active>", alert.text)
        alert.accept()
        time.sleep(0.1)
        alert = self.driver.switch_to.alert
        self.assertEqual("Hi!", alert.text)
        alert.accept()
        button = self.xpath('span[1]/button')
        button.click()
        txt = self.body_text()
        self.assertEqual("Hi! Click me! Success", txt)

