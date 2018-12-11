import unittest
import base

class Suite(base.Base):
    def test_99(self):
        """Test case: substring (1)"""
        self.start('Utf8/substrings')
        
    def test_98(self):
        """Test case: strlen (2)"""
        self.start('Utf8/strlens')
        
    def test_97(self):
        """Test case: strlenGe (3)"""
        self.start('Utf8/strlenGens')

    def test_96(self):
        """Test case: strcat (4)"""
        self.start('Utf8/strcats')

    def test_95(self):
        """Test case: strsub (5)"""
        self.start('Utf8/strsubs')

    def test_94(self):
        """Test case: strsuffix (6)"""
        self.start('Utf8/strsuffixs')

    def test_93(self):
        """Test case: strchr (7)"""
        self.start('Utf8/strchrs')
        
    def test_92(self):
        """Test case: strindex (8)"""
        self.start('Utf8/strindexs')

    def test_91(self):
        """Test case: strindex (9)"""
        self.start('Utf8/strsindexs')

    def test_90(self):
        """Test case: strcspn (10)"""
        self.start('Utf8/strcspns')

    def test_89(self):
        """Test case: str1 (11)"""
        self.start('Utf8/str1s')

    def test_88(self):
        """Test case: isalnum (12)"""
        self.start('Utf8/isalnums')
        
    def test_13(self):
        """Test case: isalpha (13)"""
        self.start('Utf8/isalphas')
        
    def test_14(self):
        """Test case: isblank (14)"""
        self.start('Utf8/isblanks')

    def test_15(self):
        """Test case: iscntrl (15)"""
        self.start('Utf8/iscntrls')

    def test_16(self):
        """Test case: isdigit (16)"""
        self.start('Utf8/isdigits')
        
    def test_17(self):
        """Test case: isgraph (17)"""
        self.start('Utf8/isgraphs')
        
    def test_18(self):
        """Test case: islower (18)"""
        self.start('Utf8/islowers')
        
    def test_19(self):
        """Test case: isprint (19)"""
        self.start('Utf8/isprints')

    def test_20(self):
        """Test case: ispunct (20)"""
        self.start('Utf8/ispuncts')

    def test_21(self):
        """Test case: isspace (21)"""
        self.start('Utf8/isspaces')

    def test_22(self):
        """Test case: isupper (22)"""
        self.start('Utf8/isuppers')

    def test_23(self):
        """Test case: isxdigit (23)"""
        self.start('Utf8/isxdigits')

    def test_24(self):
        """Test case: toupper (24)"""
        self.start('Utf8/touppers')

    def test_25(self):
        """Test case: ord (25)"""
        self.start('Utf8/ord_and_chrs')

    def test_26 (self):
        """Test case: test_db (26) """
        self.start('Utf8/test_db')
        
    def full_test (self, name):

        gap = 1000
        i = 0
        while (i + gap < 130000):        
            self.start('Utf8/' + name + '/' + str(i) + '/' + str(i + gap))
            errors = self.body_text()
            self.assertEqual("", errors, errors)
            i = i + gap


    def test_1 (self):
        """Test case: ftTolower """
        self.full_test("ftTolower")

    def test_2 (self):
        """Test case: ftToupper """
        self.full_test("ftToupper")

    def test_3 (self):
        """Test case: ftIsalpha """
        self.full_test("ftIsalpha")

    def test_4 (self):
        """Test case: ftIsdigit """
        self.full_test("ftIsdigit")
    
    def test_5 (self):
        """Test case: ftIsalnum """
        self.full_test("ftIsalnum")

    def test_6 (self):
        """Test case: ftIsspace """
        self.full_test("ftIsspace")

    def test_7 (self):
        """Test case: ftIsblank """
        self.full_test("ftIsblank")
        
    def test_8 (self):
        """Test case: ftIsprint """
        self.full_test("ftIsprint")

    def test_9 (self):
        """Test case: ftIsxdigit """
        self.full_test("ftIsxdigit")

    def test_10 (self):
        """Test case: ftIsupper """
        self.full_test("ftIsupper")
        
    def test_11 (self):
        """Test case: ftIslower """
        self.full_test("ftIslower")
