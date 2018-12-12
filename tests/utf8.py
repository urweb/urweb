import unittest
import base

class Suite(base.Base):
    
    def no_falses(self, name):
        self.start('Utf8/' + name)

        elems = self.driver.find_elements_by_xpath('//pre')

        self.assertNotEqual(0, len(elems))        
        for e in elems:
            self.assertEqual("True", e.text)
    
    def test_1(self):
        """Test case: substring (1)"""
        self.no_falses('substrings')

    def test_2(self):
        """Test case: strlen (2)"""
        self.no_falses('strlens')
        
    def test_3(self):
        """Test case: strlenGe (3)"""
        self.no_falses('strlenGens')

    def test_4(self):
        """Test case: strcat (4)"""
        self.no_falses('strcats')

    def test_5(self):
        """Test case: strsub (5)"""
        self.no_falses('strsubs')

    def test_6(self):
        """Test case: strsuffix (6)"""
        self.no_falses('strsuffixs')

    def test_7(self):
        """Test case: strchr (7)"""
        self.no_falses('strchrs')
        
    def test_8(self):
        """Test case: strindex (8)"""
        self.no_falses('strindexs')

    def test_9(self):
        """Test case: strindex (9)"""
        self.no_falses('strsindexs')

    def test_10(self):
        """Test case: strcspn (10)"""
        self.no_falses('strcspns')

    def test_11(self):
        """Test case: str1 (11)"""
        self.no_falses('str1s')

    def test_12(self):
        """Test case: isalnum (12)"""
        self.no_falses('isalnums')
        
    def test_13(self):
        """Test case: isalpha (13)"""
        self.no_falses('isalphas')
        
    def test_14(self):
        """Test case: isblank (14)"""
        self.no_falses('isblanks')

    def test_15(self):
        """Test case: iscntrl (15)"""
        self.no_falses('iscntrls')

    def test_16(self):
        """Test case: isdigit (16)"""
        self.no_falses('isdigits')
        
    def test_17(self):
        """Test case: isgraph (17)"""
        self.no_falses('isgraphs')
        
    def test_18(self):
        """Test case: islower (18)"""
        self.no_falses('islowers')
        
    def test_19(self):
        """Test case: isprint (19)"""
        self.no_falses('isprints')

    def test_20(self):
        """Test case: ispunct (20)"""
        self.no_falses('ispuncts')

    def test_21(self):
        """Test case: isspace (21)"""
        self.no_falses('isspaces')

    def test_22(self):
        """Test case: isupper (22)"""
        self.no_falses('isuppers')

    def test_23(self):
        """Test case: isxdigit (23)"""
        self.no_falses('isxdigits')

    def test_24(self):
        """Test case: toupper (24)"""
        self.no_falses('touppers')

    def test_25(self):
        """Test case: ord (25)"""
        self.no_falses('ord_and_chrs')

    def test_26 (self):
        """Test case: test_db (26) """
        self.no_falses('test_db')
        
    def full_test (self, name):

        gap = 1000
        i = 0
        while (i + gap < 130000):        
            self.start('Utf8/' + name + '/' + str(i) + '/' + str(i + gap))
            errors = self.body_text()
            self.assertEqual("", errors, errors)
            i = i + gap


    def test_89 (self):
        """Test case: ftTolower """
        self.full_test("ftTolower")

    def test_90 (self):
        """Test case: ftToupper """
        self.full_test("ftToupper")

    def test_91 (self):
        """Test case: ftIsalpha """
        self.full_test("ftIsalpha")

    def test_92 (self):
        """Test case: ftIsdigit """
        self.full_test("ftIsdigit")
    
    def test_93 (self):
        """Test case: ftIsalnum """
        self.full_test("ftIsalnum")

    def test_94 (self):
        """Test case: ftIsspace """
        self.full_test("ftIsspace")

    def test_95 (self):
        """Test case: ftIsblank """
        self.full_test("ftIsblank")
        
    def test_96 (self):
        """Test case: ftIsprint """
        self.full_test("ftIsprint")

    def test_97 (self):
        """Test case: ftIsxdigit """
        self.full_test("ftIsxdigit")

    def test_98 (self):
        """Test case: ftIsupper """
        self.full_test("ftIsupper")
        
    def test_99 (self):
        """Test case: ftIslower """
        self.full_test("ftIslower")
    '''
    '''
