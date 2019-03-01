import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('main')
        reg = self.xpath('button')
        # click a couple of times
        reg.click()
        reg.click()
        # we should get HTML spliced into HTML as-is (properly escaped even!)
        span = self.xpath('span')
        txt = span.text
        self.assertRegex(txt, ".*\\(0\\).* :: .*\\(1\\).* :: \\[\\]")
