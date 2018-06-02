import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('Filter/main')
        tx = self.body_text()
        self.assertEqual("4, 4; 44, 4.4;", tx)
