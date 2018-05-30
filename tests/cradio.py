import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start("Cradio/main")
        txt = self.xpath('div[1]').text
        self.assertEqual("Hello, I'm B. I'll be your waiter for this evening.", txt)
        txt2 = self.xpath('div[2]').text
        self.assertEqual('Value:', txt2)
        el1 = self.xpath('label[1]/input')
        el2 = self.xpath('label[2]/input')
        self.assertEqual(False, el1.is_selected())
        self.assertEqual(True, el2.is_selected())
        el1.click()
        alert = self.driver.switch_to.alert
        self.assertEqual("Now it's A", alert.text)
        alert.accept()
        self.assertEqual(True, el1.is_selected())
        self.assertEqual(False, el2.is_selected())
        txt = self.xpath('div[1]').text
        self.assertEqual("Hello, I'm A. I'll be your waiter for this evening.", txt)
        txt2 = self.xpath('div[2]').text
        self.assertEqual('Value:', txt2)
        # now check that the second radio group works as well
        el3 = self.xpath('label[4]/input')
        el3.click()
        alert = self.driver.switch_to.alert
        alert.accept()
        txt2 = self.xpath('div[2]').text
        self.assertEqual('Value: Y', txt2)
        self.assertEqual("Hello, I'm A. I'll be your waiter for this evening.", txt)
