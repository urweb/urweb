import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('Aborter2/main')
        self.assertEqual("", self.driver.title)
        txt = self.body_text()
        self.assertEqual("Result: 0", txt)

