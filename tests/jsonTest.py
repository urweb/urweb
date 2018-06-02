import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()

        pre = self.xpath('pre[1]')
        self.assertEqual('line 1\nline 2', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('1 :: 2 :: 3 :: []', pre.text)

        pre = self.xpath('pre[3]')
        self.assertEqual('["hi","bye\\"","hehe"]', pre.text)
