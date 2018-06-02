import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        b = self.driver.find_element_by_xpath('/html/body')
        self.assertEqual('3628800, 3628800', b.text)

