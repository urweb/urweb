import unittest
import base

class Suite(base.Base):
    def test_1(self):
        """Test case: substring (1)"""
        self.start('Utf8/substrings')

        pre = self.xpath('pre[1]')
        self.assertEqual('abc', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('bc', pre.text)

        pre = self.xpath('pre[3]')
        self.assertEqual('c', pre.text)

        pre = self.xpath('pre[4]')
        self.assertEqual('ábó', pre.text)

        pre = self.xpath('pre[5]')
        self.assertEqual('bó', pre.text)

        pre = self.xpath('pre[6]')
        self.assertEqual('ó', pre.text)
        
        pre = self.xpath('pre[7]')
        self.assertEqual('çãó', pre.text)

        pre = self.xpath('pre[8]')
        self.assertEqual('ãó', pre.text)

        pre = self.xpath('pre[9]')
        self.assertEqual('ó', pre.text)

        pre = self.xpath('pre[10]')
        self.assertEqual('', pre.text)

        pre = self.xpath('pre[11]')
        self.assertEqual('', pre.text)

        
    def test_2(self):
        """Test case: strlen (2)"""
        self.start('Utf8/strlens')

        pre = self.xpath('pre[1]')
        self.assertEqual('3', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('3', pre.text)

        pre = self.xpath('pre[3]')
        self.assertEqual('3', pre.text)

        pre = self.xpath('pre[4]')
        self.assertEqual('3', pre.text)

        pre = self.xpath('pre[5]')
        self.assertEqual('1', pre.text)

        pre = self.xpath('pre[6]')
        self.assertEqual('1', pre.text)

        pre = self.xpath('pre[7]')
        self.assertEqual('0', pre.text)
        
        pre = self.xpath('pre[8]')
        self.assertEqual('1', pre.text)
        
        pre = self.xpath('pre[9]')
        self.assertEqual('1', pre.text)

        pre = self.xpath('pre[10]')
        self.assertEqual('1', pre.text)

        pre = self.xpath('pre[11]')
        self.assertEqual('6', pre.text)

        pre = self.xpath('pre[12]')
        self.assertEqual('2', pre.text)

        pre = self.xpath('pre[13]')
        self.assertEqual('14', pre.text)

        
    def test_3(self):
        """Test case: strlenGe (3)"""
        self.start('Utf8/strlenGens')
        
        pre = self.xpath('pre[1]')
        self.assertEqual('False', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('True', pre.text)

        pre = self.xpath('pre[3]')
        self.assertEqual('False', pre.text)

        pre = self.xpath('pre[4]')
        self.assertEqual('True', pre.text)

        pre = self.xpath('pre[5]')
        self.assertEqual('True', pre.text)

        pre = self.xpath('pre[6]')
        self.assertEqual('False', pre.text)

        pre = self.xpath('pre[7]')
        self.assertEqual('True', pre.text)

        pre = self.xpath('pre[8]')
        self.assertEqual('True', pre.text)

    def test_4(self):
        """Test case: strcat (4)"""
        self.start('Utf8/strcats')
        
        pre = self.xpath('pre[1]')
        self.assertEqual('', pre.text)
        
        pre = self.xpath('pre[2]')
        self.assertEqual('0', pre.text)

        pre = self.xpath('pre[3]')
        self.assertEqual('aabb', pre.text)
        
        pre = self.xpath('pre[4]')
        self.assertEqual('4', pre.text)
        
        pre = self.xpath('pre[5]')
        self.assertEqual('bb', pre.text)
        
        pre = self.xpath('pre[6]')
        self.assertEqual('2', pre.text)
        
        pre = self.xpath('pre[7]')
        self.assertEqual('aa', pre.text)
        
        pre = self.xpath('pre[8]')
        self.assertEqual('2', pre.text)
        
        pre = self.xpath('pre[9]')
        self.assertEqual('ààáá', pre.text)
        
        pre = self.xpath('pre[10]')
        self.assertEqual('4', pre.text)
        
        pre = self.xpath('pre[11]')
        self.assertEqual('áá', pre.text)
        
        pre = self.xpath('pre[12]')
        self.assertEqual('2', pre.text)
        
        pre = self.xpath('pre[13]')
        self.assertEqual('àà', pre.text)
        
        pre = self.xpath('pre[14]')
        self.assertEqual('2', pre.text)

    def test_5(self):
        """Test case: strsub (5)"""
        self.start('Utf8/strsubs')

        pre = self.xpath('pre[1]')
        self.assertEqual('a', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('b', pre.text)

        pre = self.xpath('pre[3]')
        self.assertEqual('à', pre.text)

        pre = self.xpath('pre[4]')
        self.assertEqual('ç', pre.text)

    def test_6(self):
        """Test case: strsuffix (6)"""
        self.start('Utf8/strsuffixs')

        pre = self.xpath('pre[1]')
        self.assertEqual('abàç', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('bàç', pre.text)
        
        pre = self.xpath('pre[3]')
        self.assertEqual('àç', pre.text)
        
        pre = self.xpath('pre[4]')
        self.assertEqual('ç', pre.text)

    def test_7(self):
        """Test case: strchr (7)"""
        self.start('Utf8/strchrs')

        pre = self.xpath('pre[1]')
        self.assertEqual('None', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('Some "bàç"', pre.text)
        
        pre = self.xpath('pre[3]')
        self.assertEqual('Some "àç"', pre.text)

        pre = self.xpath('pre[4]')
        self.assertEqual('Some "ç"', pre.text)

        pre = self.xpath('pre[5]')
        self.assertEqual('Some ""', pre.text)
        
    def test_8(self):
        """Test case: strindex (8)"""
        self.start('Utf8/strindexs')
        
        pre = self.xpath('pre[1]')
        self.assertEqual('None', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('Some 0', pre.text)
        
        pre = self.xpath('pre[3]')
        self.assertEqual('Some 1', pre.text)

        pre = self.xpath('pre[4]')
        self.assertEqual('Some 2', pre.text)

        pre = self.xpath('pre[5]')
        self.assertEqual('Some 3', pre.text)

    def test_9(self):
        """Test case: strindex (9)"""
        self.start('Utf8/strsindexs')

        pre = self.xpath('pre[1]')
        # behavior of strstr C function
        self.assertEqual('Some 0', pre.text)
        
        pre = self.xpath('pre[2]')
        self.assertEqual('Some 0', pre.text)
        
        pre = self.xpath('pre[3]')
        self.assertEqual('None', pre.text)
        
        pre = self.xpath('pre[4]')
        self.assertEqual('Some 1', pre.text)

        pre = self.xpath('pre[5]')
        self.assertEqual('None', pre.text)
        
        pre = self.xpath('pre[6]')
        self.assertEqual('Some 2', pre.text)

        pre = self.xpath('pre[7]')
        self.assertEqual('None', pre.text)
        
        pre = self.xpath('pre[8]')
        self.assertEqual('None', pre.text)

        pre = self.xpath('pre[9]')
        self.assertEqual('Some 3', pre.text)

    def test_10(self):
        """Test case: strcspn (10)"""
        self.start('Utf8/strcspns')

        pre = self.xpath('pre[1]')
        self.assertEqual('4', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('0', pre.text)
        
        pre = self.xpath('pre[3]')
        self.assertEqual('0', pre.text)
        
        pre = self.xpath('pre[4]')
        self.assertEqual('1', pre.text)

        pre = self.xpath('pre[5]')
        self.assertEqual('2', pre.text)

        pre = self.xpath('pre[6]')
        self.assertEqual('3', pre.text)

    def test_11(self):
        """Test case: str1 (11)"""
        self.start('Utf8/str1s')

        pre = self.xpath('pre[1]')
        self.assertEqual('a', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('à', pre.text)

        pre = self.xpath('pre[3]')
        self.assertEqual('á', pre.text)

    def test_12(self):
        """Test case: isalnum (12)"""
        self.start('Utf8/isalnums')
                               
        for idx in range(1, 9):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isalnum: assert ' + str(idx))
        
    def test_13(self):
        """Test case: isalpha (13)"""
        self.start('Utf8/isalphas')
                       
        for idx in range(1, 9):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isalpha: assert ' + str(idx))
        
    def test_14(self):
        """Test case: isblank (14)"""
        self.start('Utf8/isblanks')
               
        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isblank: assert ' + str(idx))

    def test_15(self):
        """Test case: iscntrl (15)"""
        self.start('Utf8/iscntrls')
               
        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed iscntrl: assert ' + str(idx))
        
    def test_16(self):
        """Test case: isdigit (16)"""
        self.start('Utf8/isdigits')
               
        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isdigit: assert ' + str(idx))

        
    def test_17(self):
        """Test case: isgraph (17)"""
        self.start('Utf8/isgraphs')
        
        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isgraph: assert ' + str(idx))
    
    def test_18(self):
        """Test case: islower (18)"""
        self.start('Utf8/islowers')
        
        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed islower: assert ' + str(idx))
        
    def test_19(self):
        """Test case: isprint (19)"""
        self.start('Utf8/isprints')

        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isprint: assert ' + str(idx))
        
    def test_20(self):
        """Test case: ispunct (20)"""
        self.start('Utf8/ispuncts')

        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed ispunct: assert ' + str(idx))
        
    def test_21(self):
        """Test case: isspace (21)"""
        self.start('Utf8/isspaces')

        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isspace: assert ' + str(idx))

    def test_22(self):
        """Test case: isupper (22)"""
        self.start('Utf8/isuppers')

        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isupper: assert ' + str(idx))

    def test_23(self):
        """Test case: isxdigit (23)"""
        self.start('Utf8/isxdigits')

        for idx in range(1, 11):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed isxdigit: assert ' + str(idx))

    def test_24(self):
        """Test case: toupper (24)"""
        self.start('Utf8/touppers')

        for idx in range(1, 6):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed toupper: assert ' + str(idx))

    def test_25(self):
        """Test case: ord (25)"""
        self.start('Utf8/ord_and_chrs')

        for idx in range(1, 8):
            pre = self.xpath('pre[' + str(idx) + ']')
            self.assertEqual('True', pre.text, 'Failed ord: assert ' + str(idx))

    def test_26 (self):
        """Test case: test_db (26) """
        self.start('Utf8/test_db')

        pre = self.xpath('pre[1]')
        self.assertEqual('abc', pre.text)

        pre = self.xpath('pre[2]')
        self.assertEqual('3', pre.text)

        pre = self.xpath('pre[3]')
        self.assertEqual('çãó', pre.text)

        pre = self.xpath('pre[4]')
        self.assertEqual('3', pre.text)

        pre = self.xpath('pre[5]')
        self.assertEqual('が', pre.text)

        pre = self.xpath('pre[6]')
        self.assertEqual('1', pre.text)

        pre = self.xpath('pre[7]')
        self.assertEqual('漢', pre.text)

        pre = self.xpath('pre[8]')
        self.assertEqual('1', pre.text)

        pre = self.xpath('pre[9]')
        self.assertEqual('カ', pre.text)

        pre = self.xpath('pre[10]')
        self.assertEqual('1', pre.text)

        pre = self.xpath('pre[11]')
        self.assertEqual('وظيفية', pre.text)

        pre = self.xpath('pre[12]')
        self.assertEqual('6', pre.text)
