import unittest
import base
import time 

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        alert = self.driver.switch_to.alert
        self.assertEqual("Hi!", alert.text)
        alert.accept()
        time.sleep(0.1)
        alert = self.driver.switch_to.alert
        self.assertEqual("Bye!", alert.text)
        alert.accept()
