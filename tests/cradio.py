import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start("Cradio/main")
        txt = self.body_text()
        self.assertEqual("Wilbur Walbur Hello, I'm B. I'll be your waiter for this evening.", txt)
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
        txt = self.body_text()
        self.assertEqual("Wilbur Walbur Hello, I'm A. I'll be your waiter for this evening.", txt)
