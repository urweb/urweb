import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.driver.get('http://localhost:8080/main')
        el = self.driver.find_element_by_xpath('/html/body')
        self.assertEqual("1, 2, hi, 2.34, 8, 9", el.text)
