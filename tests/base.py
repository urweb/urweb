# use pip install selenium first
# ensure you have both chome driver & chrome installed

import unittest
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException

class Base(unittest.TestCase):
    """Include test cases on a given url"""

    def start(self, path='main'):
        self.driver.get('http://localhost:8080/' + path)
    def xpath(self, path):
        return self.driver.find_element_by_xpath('/html/body/'+path)
    def body_text(self):
        return self.driver.find_element_by_xpath('/html/body').text

    def setUp(self):
        """Start web driver"""
        chrome_options = webdriver.ChromeOptions()
        chrome_options.add_argument('--no-sandbox')
        chrome_options.add_argument('--headless')
        chrome_options.add_argument('--disable-gpu')
        self.driver = webdriver.Chrome(options=chrome_options)
        self.driver.implicitly_wait(10)

    def tearDown(self):
        """Stop web driver"""
        self.driver.quit()
