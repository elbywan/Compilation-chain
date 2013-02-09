typedef void** (*fp)(void**,void**);


void ** f206(void ** x1, void ** max);
void ** f208(void ** x1, void ** moy);
void ** f209(void ** x1, void ** a);
void ** f210(void ** x1, void ** count);
void ** f212(void ** x1, void ** x13);
void ** f215(void ** x1, void ** x136);
void ** f213(void ** x1, void ** x214);
void ** f211(void ** x1, void ** x14);
void ** f192(void ** x1, void ** a);
void ** f193(void ** x1, void ** elem);
void ** f200(void ** x1, void ** x135);
void ** f202(void ** x1, void ** x11);
void ** f204(void ** x1, void ** x9);
void ** f203(void ** x1, void ** x10);
void ** f201(void ** x1, void ** x12);
void ** f197(void ** x1, void ** x134);
void ** f199(void ** x1, void ** x7);
void ** f198(void ** x1, void ** x8);
void ** f196(void ** x1, void ** x133);
void ** f194(void ** x1, void ** x195);
void ** f180(void ** x1, void ** a);
void ** f181(void ** x1, void ** nb);
void ** f188(void ** x1, void ** x132);
void ** f190(void ** x1, void ** x5);
void ** f189(void ** x1, void ** x6);
void ** f185(void ** x1, void ** x131);
void ** f187(void ** x1, void ** x3);
void ** f186(void ** x1, void ** x4);
void ** f184(void ** x1, void ** x130);
void ** f182(void ** x1, void ** x183);void ** f206(void ** x1, void ** max){  
void** moy = ((int) (max)) / ((int) ((int) 2.)); 
  void** newabr = ((fp) ((void **) ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), (void * []) { 
                  0 })[0])) (((void **) ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), (void * []) { 
                  0 })[1]), moy); 
  void** x207 = (void * []) { 0 }; 
  void** loop = (void * []) { f208, x207 }; 
  void** x216 = x207; x207[0] = loop; 
  ((fp) ((void **) ((fp) ((void **) ((fp) ((void **) loop[0])) (((void **) loop[1]), moy)[0])) (((void **) ((fp) ((void **) loop[0])) (((void **) loop[1]), moy)[1]), newabr)[0])) (((void **) ((fp) ((void **) ((fp) ((void **) loop[0])) (((void **) loop[1]), moy)[0])) (((void **) ((fp) ((void **) loop[0])) (((void **) loop[1]), moy)[1]), newabr)[1]), max) ;
 } 
void ** f208(void ** x1, void ** moy){  
(void * []) { f209, (void * []) { moy, ((void **) x1[0]) } } ; } 
void ** f209(void ** x1, void ** a){  
(void * []) { f210, (void * []) { a, ((void **) x1[0]), ((void **) x1[1]) } } ;
 } 
void ** f210(void ** x1, void ** count){  
if (((int) ((int) 1.)) < ((int) (count)) == 0){ 
  (void * []) { f211, (void * []) { ((void **) x1[0]) } }; } else { (void * []) { 
                                                                    f212, 
                                                                    (void * []) { 
                                                                    ((void **) x1[0]), 
                                                                    ((void **) x1[1]), 
                                                                    ((void **) x1[2]), 
                                                                    count } }; } ;
 } 
void ** f212(void ** x1, void ** x13){  
if (((int) (((int) (1)) - ((int) (((void **) ((void **) x1[0])[0]))))) + ((int) (0)) == 0){ 
  void** i = ((void **) ((void **) x1[0])[1]); 
  (void * []) { f215, 
                (void * []) { i, ((void **) x1[1]), ((void **) x1[2]), 
                              ((void **) x1[3]) } }; } else { 0; } ;
 } 
void ** f215(void ** x1, void ** x136){  
void** left = ((fp) ((void **) ((fp) ((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((int) (((void **) x1[1]))) / ((int) ((int) 2.)))[0])) (((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((int) (((void **) x1[1]))) / ((int) ((int) 2.)))[1]), (void * []) { 
              1, 
              ((int) (((void **) x1[0]))) - ((int) (((int) (((void **) x1[1]))) / ((int) ((int) 2.)))) })[0])) (((void **) ((fp) ((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((int) (((void **) x1[1]))) / ((int) ((int) 2.)))[0])) (((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((int) (((void **) x1[1]))) / ((int) ((int) 2.)))[1]), (void * []) { 
              1, 
              ((int) (((void **) x1[0]))) - ((int) (((int) (((void **) x1[1]))) / ((int) ((int) 2.)))) })[1]), ((int) (((void **) x1[3]))) / ((int) ((int) 2.))); 
  void** right = ((fp) ((void **) ((fp) ((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((int) (((void **) x1[1]))) / ((int) ((int) 2.)))[0])) (((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((int) (((void **) x1[1]))) / ((int) ((int) 2.)))[1]), (void * []) { 
                 1, 
                 ((int) (((void **) x1[0]))) + ((int) (((int) (((void **) x1[1]))) / ((int) ((int) 2.)))) })[0])) (((void **) ((fp) ((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((int) (((void **) x1[1]))) / ((int) ((int) 2.)))[0])) (((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((int) (((void **) x1[1]))) / ((int) ((int) 2.)))[1]), (void * []) { 
                 1, 
                 ((int) (((void **) x1[0]))) + ((int) (((int) (((void **) x1[1]))) / ((int) ((int) 2.)))) })[1]), ((int) (((void **) x1[3]))) / ((int) ((int) 2.))); 
  (void * []) { 2, ((void **) x1[0]), left, right } ;
 } void ** f213(void ** x1, void ** x214){  x1 ; } 
void ** f211(void ** x1, void ** x14){  ((void **) x1[0]) ; } 
void ** f192(void ** x1, void ** a){  
(void * []) { f193, (void * []) { a, ((void **) x1[0]) } } ; } 
void ** f193(void ** x1, void ** elem){  
if (((int) (((int) (0)) - ((int) (((void **) ((void **) x1[0])[0]))))) + ((int) (0)) == 0){ 
  (void * []) { f196, (void * []) {  } }; } else { if (((int) (((int) (1)) - ((int) (((void **) ((void **) x1[0])[0]))))) + ((int) (0)) == 0){ 
                                                   void** i = ((void **) ((void **) x1[0])[1]); 
                                                   (void * []) { f194, 
                                                                 (void * []) { 
                                                                 f197, 
                                                                 (void * []) { 
                                                                 i, elem } } }; } else { 
                                                   if (((int) (((int) (2)) - ((int) (((void **) ((void **) x1[0])[0]))))) + ((int) (0)) == 0){ 
                                                   void** i = ((void **) ((void **) x1[0])[1]); 
                                                   void** g = ((void **) ((void **) x1[0])[2]); 
                                                   void** d = ((void **) ((void **) x1[0])[3]); 
                                                   (void * []) { f194, 
                                                                 (void * []) { 
                                                                 f194, 
                                                                 (void * []) { 
                                                                 f200, 
                                                                 (void * []) { 
                                                                 i, g, d, 
                                                                 ((void **) x1[1]), 
                                                                 elem } } } }; } else { 
                                                   0; }; }; } ;
 } 
void ** f200(void ** x1, void ** x135){  
if (((int) (((void **) x1[0]))) < ((int) (((void **) x1[4]))) == 0){ 
  (void * []) { f201, 
                (void * []) { ((void **) x1[1]), ((void **) x1[3]), 
                              ((void **) x1[4]) } }; } else { (void * []) { 
                                                              f202, 
                                                              (void * []) { 
                                                              ((void **) x1[0]), 
                                                              ((void **) x1[2]), 
                                                              ((void **) x1[3]), 
                                                              ((void **) x1[4]) } }; } ;
 } 
void ** f202(void ** x1, void ** x11){  
if (((int) ((int) 0.)) < ((int) (((int) (((void **) x1[0]))) - ((int) (((void **) x1[3]))))) == 0){ 
  (void * []) { f203, (void * []) {  } }; } else { (void * []) { f204, 
                                                                 (void * []) { 
                                                                 ((void **) x1[1]), 
                                                                 ((void **) x1[2]), 
                                                                 ((void **) x1[3]) } }; } ;
 } 
void ** f204(void ** x1, void ** x9){  
((fp) ((void **) ((fp) ((void **) ((void **) x1[1])[0])) (((void **) ((void **) x1[1])[1]), ((void **) x1[0]))[0])) (((void **) ((fp) ((void **) ((void **) x1[1])[0])) (((void **) ((void **) x1[1])[1]), ((void **) x1[0]))[1]), ((void **) x1[2])) ;
 } void ** f203(void ** x1, void ** x10){  1 ; } 
void ** f201(void ** x1, void ** x12){  
((fp) ((void **) ((fp) ((void **) ((void **) x1[1])[0])) (((void **) ((void **) x1[1])[1]), ((void **) x1[0]))[0])) (((void **) ((fp) ((void **) ((void **) x1[1])[0])) (((void **) ((void **) x1[1])[1]), ((void **) x1[0]))[1]), ((void **) x1[2])) ;
 } 
void ** f197(void ** x1, void ** x134){  
if (((int) ((int) 0.)) < ((int) (((int) (((void **) x1[0]))) - ((int) (((void **) x1[1]))))) == 0){ 
  (void * []) { f198, (void * []) {  } }; } else { (void * []) { f199, 
                                                                 (void * []) { 
                                                                  } }; } ;
 } void ** f199(void ** x1, void ** x7){  0 ; } 
void ** f198(void ** x1, void ** x8){  1 ; } 
void ** f196(void ** x1, void ** x133){  0 ; } 
void ** f194(void ** x1, void ** x195){  x1 ; } 
void ** f180(void ** x1, void ** a){  
(void * []) { f181, (void * []) { ((void **) x1[0]), a } } ; } 
void ** f181(void ** x1, void ** nb){  
if (((int) (((int) (0)) - ((int) (((void **) ((void **) x1[1])[0]))))) + ((int) (0)) == 0){ 
  (void * []) { f184, (void * []) { nb } }; } else { if (((int) (((int) (1)) - ((int) (((void **) ((void **) x1[1])[0]))))) + ((int) (0)) == 0){ 
                                                     void** i = ((void **) ((void **) x1[1])[1]); 
                                                     (void * []) { f182, 
                                                                   (void * []) { 
                                                                   f185, 
                                                                   (void * []) { 
                                                                   nb, i } } }; } else { 
                                                     if (((int) (((int) (2)) - ((int) (((void **) ((void **) x1[1])[0]))))) + ((int) (0)) == 0){ 
                                                     void** i = ((void **) ((void **) x1[1])[1]); 
                                                     void** g = ((void **) ((void **) x1[1])[2]); 
                                                     void** d = ((void **) ((void **) x1[1])[3]); 
                                                     (void * []) { f182, 
                                                                   (void * []) { 
                                                                   f182, 
                                                                   (void * []) { 
                                                                   f188, 
                                                                   (void * []) { 
                                                                   ((void **) x1[0]), 
                                                                   nb, i, g, 
                                                                   d } } } }; } else { 
                                                     0; }; }; } ;
 } 
void ** f188(void ** x1, void ** x132){  
if (((int) (((void **) x1[2]))) < ((int) (((void **) x1[1]))) == 0){ 
  (void * []) { f189, 
                (void * []) { ((void **) x1[0]), ((void **) x1[1]), 
                              ((void **) x1[2]), ((void **) x1[3]), 
                              ((void **) x1[4]) } }; } else { (void * []) { 
                                                              f190, 
                                                              (void * []) { 
                                                              ((void **) x1[0]), 
                                                              ((void **) x1[1]), 
                                                              ((void **) x1[2]), 
                                                              ((void **) x1[3]), 
                                                              ((void **) x1[4]) } }; } ;
 } 
void ** f190(void ** x1, void ** x5){  
(void * []) { 2, ((void **) x1[2]), ((void **) x1[3]), 
              ((fp) ((void **) ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((void **) x1[4]))[0])) (((void **) ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((void **) x1[4]))[1]), ((void **) x1[1])) } ;
 } 
void ** f189(void ** x1, void ** x6){  
(void * []) { 2, ((void **) x1[2]), 
              ((fp) ((void **) ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((void **) x1[3]))[0])) (((void **) ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((void **) x1[3]))[1]), ((void **) x1[1])), 
              ((void **) x1[4]) } ;
 } 
void ** f185(void ** x1, void ** x131){  
if (((int) (((void **) x1[1]))) < ((int) (((void **) x1[0]))) == 0){ 
  (void * []) { f186, (void * []) { ((void **) x1[0]), ((void **) x1[1]) } }; } else { 
  (void * []) { f187, (void * []) { ((void **) x1[0]), ((void **) x1[1]) } }; } ;
 } 
void ** f187(void ** x1, void ** x3){  
(void * []) { 2, ((void **) x1[0]), (void * []) { 1, ((void **) x1[1]) }, 
              (void * []) { 0 } } ;
 } 
void ** f186(void ** x1, void ** x4){  
(void * []) { 2, ((void **) x1[0]), (void * []) { 0 }, 
              (void * []) { 1, ((void **) x1[1]) } } ;
 } 
void ** f184(void ** x1, void ** x130){  (void * []) { 1, ((void **) x1[0]) } ;
 } void ** f182(void ** x1, void ** x183){  x1 ; } 

int main(){ 
 void** x179 = (void * []) { 0 }; 
 void** ajout = (void * []) { f180, x179 }; 
 void** x218 = x179; x179[0] = ajout; 
 void** x191 = (void * []) { 0 }; 
 void** isIn = (void * []) { f192, x191 }; 
 void** x217 = x191; x191[0] = isIn; 
 void** x205 = (void * []) { ajout }; 
 void** cree_arbre_equilibre = (void * []) { f206, x205 }; 
 ((fp) ((void **) cree_arbre_equilibre[0])) (((void **) cree_arbre_equilibre[1]), (int) 10.); 
}
