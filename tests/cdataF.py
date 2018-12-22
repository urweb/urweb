import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('main')
        self.assertEqual("<Hi.\nBye.", self.body_text())
