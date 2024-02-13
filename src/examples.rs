//! Examples copied from the paper for testing

use lazy_static::lazy_static;

#[rustfmt::skip] 
lazy_static! {
    /// extract the quantity of the purchase
    pub static ref EXAMPLE2: [[&'static str; 2]; 5] = [
        ["BTR KRNL WK CORN 15Z"     , "15Z"],
        ["CAMP DRY DBL NDL 3.6 OZ"  , "3.6 OZ"],
        ["CHORE BOY HD SC SPNG 1 PK", "1 PK"],
        ["FRENCH WORCESTERSHIRE 5 Z", "5 Z"],
        ["O F TOMATO PASTE 6 OZ"    , "6 OZ"]
    ];
    /// extract directory name from a path
    pub static ref EXAMPLE3: [[&'static str; 2]; 2] = [
        ["Company/Code/index.html"      , "Company/Code/"],
        ["Company/Docs/Spec/specs.doc"  , "Company/Docs/Spec/"]
    ];    
    /// generate abbreviations of a string
    pub static ref EXAMPLE4: [[&'static str; 2]; 3] = [
        ["International Business Machines"                  , "IBM"],
        ["Principle Of Programming Languages"               , "POPL"],
        ["Internaltional Conference on Software Engineering", "ICSE"]
    ];
    pub static ref EXAMPLE5: [[&'static str; 2]; 3] = [
        ["(6/7)(4/5)(14/1)" , "6/7# 4/5# 14/1# "],
        ["49(28/11)(14/1)"  , "28/11# 14/1# "],
        ["() (28/11)(14/1)" , "28/11# 14/1# "],
    ];
    /// remove excess spaces
    pub static ref EXAMPLE6: [[&'static str; 2]; 2] = [
        ["  Oege   de    Moor"      , "Oege de Moor"],
        ["Kathleen Fisher AT&T Labs", "Kathleen Fisher AT&T Labs"],
    ];
    /// conditional concatenation
    pub static ref EXAMPLE7: [[&'static str; 3]; 4] = [
        ["Alex" , "Asst."   , "Alex(Asst.)"],
        ["Jim"  , "Manager" , "Jim(Manager)"],
        ["Ryan" , ""        , ""],
        [""     , "Asst."   , ""],
    ];
    /// mixed date parsing
    pub static ref EXAMPLE8: [[&'static str; 2]; 3] = [
        ["01/21/2001", "01"],
        ["22.02.2002", "02"],
        ["2003-23-03", "03"],
    ];
    /// name parsing
    pub static ref EXAMPLE9: [[&'static str; 2]; 5] = [
        ["Dr. Eran Yahav"           , "Yahav, E."],
        ["Prof. Kathleen S. Fisher" , "Fisher, K."],
        ["Bill Gates, Sr."          , "Gates, B."],
        ["George Ciprian Necula"    , "Necula, G."],
        ["Ken McMillan, II"         , "McMillan, K."],
    ];
    /// phone number
    pub static ref EXAMPLE10: [[&'static str; 2]; 5] = [
        ["323-708-7700"     , "323-708-7700"],
        ["(425)-706-7709"   , "425-706-7709"],
        ["510.220.5586"     , "510-220-5586"],
        ["235 7654"         , "425-235-7654"],
        ["745-8139"         , "425-745-8139"],
    ];
    /// Synthesis future extension
    pub static ref EXAMPLE12: [[&'static str; 3]; 2] = [
        ["Albania", "355", "case 355: return \"Albania\";"],
        ["Algeria", "213", "case 213: return \"Algeria\";"],
    ];
    /// filter task
    pub static ref EXAMPLE13: [[&'static str; 3]; 6] = [
        ["/um/people/sumitg/pictures/lake-tahoe/index.html" , "192", "192"],
        ["/um/people/sumitg/index.html"                     , "104", "0"],
        ["/um/people/sumitg/pubs/speed.html"                , "16", "0"],
        ["/um/people/sumitg/pubs/popl10_synthesis.pdf"      , "13", "0"],
        ["/um/people/sumitg/pictures/verona/index.html"     , "7", "7"],
        ["/um/people/sumitg/pictures/kerela/target21.html"  , "3", "3"],
    ];
    pub static ref EXAMPLE14: [[&'static str; 2]; 2] = [
        ["Alpha 10 Beta 20 Charlie 30 Delta" , "10+20+30"],
        ["POPL 9 CAV 7 PLDI 6 ESOP 4       " , "9+7+6+4"],
    ];
}
