import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        alert = self.driver.switch_to.alert
        self.assertEqual("Howdy, neighbor!", alert.text)
        alert.accept()
        txt = self.body_text()
        self.assertEqual("This one ain't empty.", txt)
