import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('Aborter/main')
        self.assertEqual("Fatal Error", self.driver.title)
        txt = self.body_text()
        self.assertEqual("Fatal error: :0:0-0:0: No way, Jose!", txt)

