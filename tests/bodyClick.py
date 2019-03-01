import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('main')
        bd = self.driver.find_element_by_xpath('/html/body')

        bd.click()
        alert = self.driver.switch_to.alert
        self.assertEqual("You clicked the body.", alert.text)
        alert.accept()

        bd.send_keys('h')
        alert = self.driver.switch_to.alert
        self.assertEqual("Key", alert.text)
        alert.accept()
