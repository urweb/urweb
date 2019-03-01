import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        el = self.xpath('goofy[@name eq "beppo" and @data-role eq "excellence"]')
        el.click()
        alert = self.driver.switch_to.alert
        self.assertEqual("You clicked it!  That's some fancy shooting!", alert.text)
