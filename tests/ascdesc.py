import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('Ascdesc/main')
        el = self.xpath('p[1]')
        self.assertEqual("1; 2; 3;", el.text)
        el = self.xpath('p[2]')
        self.assertEqual("3; 2; 1;", el.text)
