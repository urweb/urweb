import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('DynChannel/main')

        # initial state: only Register is visible
        reg = self.xpath('button')
        reg.click()
        # and we get two another state: either Register or Send visible
        send = self.xpath('span/button')
        send.click()
        alert = self.driver.switch_to.alert
        self.assertEqual("Got something from the channel", alert.text)
        alert.accept()
        # we got the message back
        span = self.xpath('span/span')
        self.assertEqual("blabla", span.text)
