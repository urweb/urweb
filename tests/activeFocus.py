import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        uw0 = self.xpath('input[2]')
        active = self.driver.switch_to.active_element
        self.assertEqual(uw0, active)
    def test_2(self):
        """Test case 2"""
        self.start('dynamic')
        btn = self.xpath('button')
        btn.click()
        uw1 = self.xpath('span/input[2]')
        active = self.driver.switch_to.active_element
        self.assertEqual(uw1, active)
