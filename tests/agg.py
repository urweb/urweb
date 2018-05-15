import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('Agg/main')
        self.assertEqual("0;1;2;\na, 50;", self.body_text())
