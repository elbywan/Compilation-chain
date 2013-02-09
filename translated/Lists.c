typedef void** (*fp)(void**,void**);


void ** f176(void ** x1, void ** x);
void ** f177(void ** x1, void ** y);
void ** f175(void ** x1, void ** x);
void ** f166(void ** x1, void ** l);
void ** f167(void ** x1, void ** funct);
void ** f168(void ** x1, void ** accu);
void ** f170(void ** x1, void ** l);
void ** f171(void ** x1, void ** acc);
void ** f173(void ** x1, void ** x87);
void ** f172(void ** x1, void ** x86);
void ** f2(void ** x1, void ** x3);
void ** f158(void ** x1, void ** l);
void ** f159(void ** x1, void ** funct);
void ** f161(void ** x1, void ** l);
void ** f163(void ** x1, void ** x85);
void ** f162(void ** x1, void ** x84);
void ** f2(void ** x1, void ** x3);
void ** f150(void ** x1, void ** first);
void ** f151(void ** x1, void ** last);
void ** f153(void ** x1, void ** i);
void ** f155(void ** x1, void ** x4);
void ** f154(void ** x1, void ** x5);
void ** f142(void ** x1, void ** l);
void ** f143(void ** x1, void ** l2);
void ** f145(void ** x1, void ** l);
void ** f147(void ** x1, void ** x83);
void ** f146(void ** x1, void ** x82);
void ** f2(void ** x1, void ** x3);
void ** f135(void ** x1, void ** l);
void ** f137(void ** x1, void ** l);
void ** f139(void ** x1, void ** x81);
void ** f138(void ** x1, void ** x80);
void ** f2(void ** x1, void ** x3);void ** f176(void ** x1, void ** x){  (void * []) { f177, (void * []) { x } } ;
 } 
void ** f177(void ** x1, void ** y){  
((int) (((void **) x1[0]))) + ((int) (y)) ; } 
void ** f175(void ** x1, void ** x){  ((int) (x)) + ((int) (1)) ; } 
void ** f166(void ** x1, void ** l){  (void * []) { f167, (void * []) { l } } ;
 } 
void ** f167(void ** x1, void ** funct){  
(void * []) { f168, (void * []) { ((void **) x1[0]), funct } } ; } 
void ** f168(void ** x1, void ** accu){  
void** x169 = (void * []) { 0, ((void **) x1[1]) }; 
  void** loop = (void * []) { f170, x169 }; 
  void** x174 = x169; x169[0] = loop; 
  ((fp) ((void **) ((fp) ((void **) loop[0])) (((void **) loop[1]), ((void **) x1[0]))[0])) (((void **) ((fp) ((void **) loop[0])) (((void **) loop[1]), ((void **) x1[0]))[1]), accu) ;
 } 
void ** f170(void ** x1, void ** l){  
(void * []) { f171, (void * []) { l, ((void **) x1[0]), ((void **) x1[1]) } } ;
 } 
void ** f171(void ** x1, void ** acc){  
if (((int) (((int) (0)) - ((int) (((void **) ((void **) x1[0])[0]))))) + ((int) (0)) == 0){ 
  (void * []) { f172, (void * []) { acc } }; } else { if (((int) (((int) (1)) - ((int) (((void **) ((void **) x1[0])[0]))))) + ((int) (0)) == 0){ 
                                                      void** x = ((void **) ((void **) x1[0])[1]); 
                                                      void** xs = ((void **) ((void **) x1[0])[2]); 
                                                      (void * []) { f2, 
                                                                    (void * []) { 
                                                                    f173, 
                                                                    (void * []) { 
                                                                    x, xs, 
                                                                    ((void **) x1[1]), 
                                                                    ((void **) x1[2]), 
                                                                    acc } } }; } else { 
                                                      0; }; } ;
 } 
void ** f173(void ** x1, void ** x87){  
((fp) ((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((void **) x1[1]))[0])) (((void **) ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((void **) x1[1]))[1]), ((fp) ((void **) ((fp) ((void **) ((void **) x1[3])[0])) (((void **) ((void **) x1[3])[1]), ((void **) x1[0]))[0])) (((void **) ((fp) ((void **) ((void **) x1[3])[0])) (((void **) ((void **) x1[3])[1]), ((void **) x1[0]))[1]), ((void **) x1[4]))) ;
 } void ** f172(void ** x1, void ** x86){  ((void **) x1[0]) ; } 
void ** f2(void ** x1, void ** x3){  x1 ; } 
void ** f158(void ** x1, void ** l){  (void * []) { f159, (void * []) { l } } ;
 } 
void ** f159(void ** x1, void ** funct){  
void** x160 = (void * []) { 0, funct }; 
  void** loop = (void * []) { f161, x160 }; 
  void** x164 = x160; x160[0] = loop; 
  ((fp) ((void **) loop[0])) (((void **) loop[1]), ((void **) x1[0])) ;
 } 
void ** f161(void ** x1, void ** l){  
if (((int) (((int) (0)) - ((int) (((void **) l[0]))))) + ((int) (0)) == 0){ 
  (void * []) { f162, (void * []) {  } }; } else { if (((int) (((int) (1)) - ((int) (((void **) l[0]))))) + ((int) (0)) == 0){ 
                                                   void** x = ((void **) l[1]); 
                                                   void** xs = ((void **) l[2]); 
                                                   (void * []) { f2, 
                                                                 (void * []) { 
                                                                 f163, 
                                                                 (void * []) { 
                                                                 x, xs, 
                                                                 ((void **) x1[0]), 
                                                                 ((void **) x1[1]) } } }; } else { 
                                                   0; }; } ;
 } 
void ** f163(void ** x1, void ** x85){  
(void * []) { 1, 
              ((fp) ((void **) ((void **) x1[3])[0])) (((void **) ((void **) x1[3])[1]), ((void **) x1[0])), 
              ((fp) ((void **) ((void **) x1[2])[0])) (((void **) ((void **) x1[2])[1]), ((void **) x1[1])) } ;
 } void ** f162(void ** x1, void ** x84){  (void * []) { 0 } ; } 
void ** f2(void ** x1, void ** x3){  x1 ; } 
void ** f150(void ** x1, void ** first){  
(void * []) { f151, (void * []) { first } } ; } 
void ** f151(void ** x1, void ** last){  
void** x152 = (void * []) { last, 0 }; 
  void** loop = (void * []) { f153, x152 }; 
  void** x156 = x152; x152[1] = loop; 
  ((fp) ((void **) loop[0])) (((void **) loop[1]), ((void **) x1[0])) ;
 } 
void ** f153(void ** x1, void ** i){  
if (((int) (((int) (((void **) x1[0]))) + ((int) (1)))) - ((int) (i)) == 0){ 
  (void * []) { f154, (void * []) {  } }; } else { (void * []) { f155, 
                                                                 (void * []) { 
                                                                 ((void **) x1[1]), 
                                                                 i } }; } ;
 } 
void ** f155(void ** x1, void ** x4){  
(void * []) { 1, ((void **) x1[1]), 
              ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((int) (((void **) x1[1]))) + ((int) (1))) } ;
 } void ** f154(void ** x1, void ** x5){  (void * []) { 0 } ; } 
void ** f142(void ** x1, void ** l){  (void * []) { f143, (void * []) { l } } ;
 } 
void ** f143(void ** x1, void ** l2){  
void** x144 = (void * []) { 0, l2 }; 
  void** aux = (void * []) { f145, x144 }; 
  void** x148 = x144; x144[0] = aux; 
  ((fp) ((void **) aux[0])) (((void **) aux[1]), ((void **) x1[0])) ;
 } 
void ** f145(void ** x1, void ** l){  
if (((int) (((int) (0)) - ((int) (((void **) l[0]))))) + ((int) (0)) == 0){ 
  (void * []) { f146, (void * []) { ((void **) x1[1]) } }; } else { if (((int) (((int) (1)) - ((int) (((void **) l[0]))))) + ((int) (0)) == 0){ 
                                                                    void** x = 
                                                                    ((void **) l[1]); 
                                                                    void** xs = 
                                                                    ((void **) l[2]); 
                                                                    (void * []) { 
                                                                    f2, 
                                                                    (void * []) { 
                                                                    f147, 
                                                                    (void * []) { 
                                                                    ((void **) x1[0]), 
                                                                    x, xs } } }; } else { 
                                                                    0; }; } ;
 } 
void ** f147(void ** x1, void ** x83){  
(void * []) { 1, ((void **) x1[1]), 
              ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((void **) x1[2])) } ;
 } void ** f146(void ** x1, void ** x82){  ((void **) x1[0]) ; } 
void ** f2(void ** x1, void ** x3){  x1 ; } 
void ** f135(void ** x1, void ** l){  
void** x136 = (void * []) { 0 }; 
  void** aux = (void * []) { f137, x136 }; 
  void** x140 = x136; x136[0] = aux; 
  ((fp) ((void **) aux[0])) (((void **) aux[1]), l) ;
 } 
void ** f137(void ** x1, void ** l){  
if (((int) (((int) (0)) - ((int) (((void **) l[0]))))) + ((int) (0)) == 0){ 
  (void * []) { f138, (void * []) {  } }; } else { if (((int) (((int) (1)) - ((int) (((void **) l[0]))))) + ((int) (0)) == 0){ 
                                                   void** x = ((void **) l[1]); 
                                                   void** xs = ((void **) l[2]); 
                                                   (void * []) { f2, 
                                                                 (void * []) { 
                                                                 f139, 
                                                                 (void * []) { 
                                                                 ((void **) x1[0]), 
                                                                 xs } } }; } else { 
                                                   0; }; } ;
 } 
void ** f139(void ** x1, void ** x81){  
((int) (1)) + ((int) (((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((void **) x1[1])))) ;
 } void ** f138(void ** x1, void ** x80){  0 ; } 
void ** f2(void ** x1, void ** x3){  x1 ; } 

int main(){ 
 void** x134 = (void * []) {  }; 
 void** length = (void * []) { f135, x134 }; 
 void** x141 = (void * []) {  }; 
 void** concat = (void * []) { f142, x141 }; 
 void** x149 = (void * []) {  }; 
 void** create_from = (void * []) { f150, x149 }; 
 void** x157 = (void * []) {  }; 
 void** map = (void * []) { f158, x157 }; 
 void** x165 = (void * []) {  }; 
 void** fold_left = (void * []) { f166, x165 }; 
 void** list1 = ((fp) ((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 5)[0])) (((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 5)[1]), 10); 
 void** list2 = ((fp) ((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 15)[0])) (((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 15)[1]), 20); 
 void** list3 = ((fp) ((void **) ((fp) ((void **) map[0])) (((void **) map[1]), ((fp) ((void **) ((fp) ((void **) concat[0])) (((void **) concat[1]), list1)[0])) (((void **) ((fp) ((void **) concat[0])) (((void **) concat[1]), list1)[1]), list2))[0])) (((void **) ((fp) ((void **) map[0])) (((void **) map[1]), ((fp) ((void **) ((fp) ((void **) concat[0])) (((void **) concat[1]), list1)[0])) (((void **) ((fp) ((void **) concat[0])) (((void **) concat[1]), list1)[1]), list2))[1]), (void * []) { 
                f175, (void * []) {  } }); 
 void** sum = ((fp) ((void **) ((fp) ((void **) ((fp) ((void **) fold_left[0])) (((void **) fold_left[1]), ((fp) ((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 0)[0])) (((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 0)[1]), 5))[0])) (((void **) ((fp) ((void **) fold_left[0])) (((void **) fold_left[1]), ((fp) ((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 0)[0])) (((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 0)[1]), 5))[1]), (void * []) { 
              f176, (void * []) {  } })[0])) (((void **) ((fp) ((void **) ((fp) ((void **) fold_left[0])) (((void **) fold_left[1]), ((fp) ((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 0)[0])) (((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 0)[1]), 5))[0])) (((void **) ((fp) ((void **) fold_left[0])) (((void **) fold_left[1]), ((fp) ((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 0)[0])) (((void **) ((fp) ((void **) create_from[0])) (((void **) create_from[1]), 0)[1]), 5))[1]), (void * []) { 
              f176, (void * []) {  } })[1]), 0); 
 ((int) (sum)) + ((int) (((fp) ((void **) length[0])) (((void **) length[1]), list1))); 
}
