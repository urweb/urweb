import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('main')
        b = self.xpath('button')

        b.click()
        alert = self.driver.switch_to.alert
        self.assertEqual("AHOY", alert.text)
        alert.accept()
