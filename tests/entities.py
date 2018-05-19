import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case 1"""
        self.start()
        p = self.xpath('p[1]')
        self.assertEqual('Hello world! & so on, © me today (8 €)', p.text)
        p = self.xpath('p[2]')
        self.assertEqual('♠ ♣ ♥ ♦', p.text)
        p = self.xpath('p[3]')
        self.assertEqual('† DANGER †', p.text)

