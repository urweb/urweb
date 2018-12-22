import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start('Cffi/main')
        l1 = self.xpath('form[1]/input')
        l1.click()

        b1 = self.xpath('button[1]')
        b1.click() # TODO: check server output somehow

        b2 = self.xpath('button[2]')
        b2.click()
        alert = self.driver.switch_to.alert
        self.assertEqual("<<Hoho>>", alert.text)
        alert.accept()

        b3 = self.xpath('button[3]')
        b3.click()
        alert = self.driver.switch_to.alert
        self.assertEqual("Hi there!", alert.text)
    def test_2(self):
        """Test case 2"""
        self.start('Cffi/main')
        l1 = self.xpath('form[2]/input')
        l1.click()

        self.assertEqual("All good.", self.body_text())
    def test_3(self):
        """Test case 3"""
        self.start('Cffi/main')
        l1 = self.xpath('form[3]/input')
        l1.click()

        self.assertRegex(self.body_text(), "^Fatal error: .*$")
