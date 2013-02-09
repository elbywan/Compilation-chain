typedef void** (*fp)(void**,void**);


void ** f24(void ** x1, void ** x);
void ** f20(void ** x1, void ** m);
void ** f21(void ** x1, void ** f);
void ** f22(void ** x1, void ** k);
void ** f23(void ** x1, void ** v);
void ** f18(void ** x1, void ** a);
void ** f19(void ** x1, void ** x);
void ** f16(void ** x1, void ** a);
void ** f17(void ** x1, void ** f);void ** f24(void ** x1, void ** x){  
((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((int) (x)) + ((int) (1))) ;
 } 
void ** f20(void ** x1, void ** m){  (void * []) { f21, (void * []) { m } } ;
 } 
void ** f21(void ** x1, void ** f){  
(void * []) { f22, (void * []) { f, ((void **) x1[0]) } } ; } 
void ** f22(void ** x1, void ** k){  
((fp) ((void **) ((void **) x1[1])[0])) (((void **) ((void **) x1[1])[1]), (void * []) { 
  f23, (void * []) { ((void **) x1[0]), k } }) ;
 } 
void ** f23(void ** x1, void ** v){  
((fp) ((void **) ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), v)[0])) (((void **) ((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), v)[1]), ((void **) x1[1])) ;
 } 
void ** f18(void ** x1, void ** a){  
((fp) ((void **) a[0])) (((void **) a[1]), (void * []) { f19, 
                                                         (void * []) { 
                                                          } }) ;
 } void ** f19(void ** x1, void ** x){  x ; } 
void ** f16(void ** x1, void ** a){  (void * []) { f17, (void * []) { a } } ;
 } 
void ** f17(void ** x1, void ** f){  
((fp) ((void **) f[0])) (((void **) f[1]), ((void **) x1[0])) ; } 

int main(){ 
 void** return = (void * []) { f16, (void * []) {  } }; 
 void** run = (void * []) { f18, (void * []) {  } }; 
 void** bind = (void * []) { f20, (void * []) {  } }; 
 ((fp) ((void **) run[0])) (((void **) run[1]), ((fp) ((void **) ((fp) ((void **) bind[0])) (((void **) bind[1]), ((fp) ((void **) return[0])) (((void **) return[1]), 1))[0])) (((void **) ((fp) ((void **) bind[0])) (((void **) bind[1]), ((fp) ((void **) return[0])) (((void **) return[1]), 1))[1]), (void * []) { 
 f24, (void * []) { return } })); 
}
