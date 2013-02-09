typedef void** (*fp)(void**,void**);


void ** f34(void ** x1, void ** l);
void ** f36(void ** x1, void ** l);
void ** f38(void ** x1, void ** x23);
void ** f37(void ** x1, void ** x22);
void ** f2(void ** x1, void ** x3);void ** f34(void ** x1, void ** l){  
void** x35 = (void * []) { 0 }; 
  void** aux = (void * []) { f36, x35 }; 
  void** x39 = x35; x35[0] = aux; 
  ((fp) ((void **) aux[0])) (((void **) aux[1]), l) ;
 } 
void ** f36(void ** x1, void ** l){  
if (((int) (((int) (0)) - ((int) (((void **) l[0]))))) + ((int) (0)) == 0){ 
  (void * []) { f37, (void * []) {  } }; } else { if (((int) (((int) (1)) - ((int) (((void **) l[0]))))) + ((int) (0)) == 0){ 
                                                  void** x = ((void **) l[1]); 
                                                  void** xs = ((void **) l[2]); 
                                                  (void * []) { f2, 
                                                                (void * []) { 
                                                                f38, 
                                                                (void * []) { 
                                                                ((void **) x1[0]), 
                                                                xs } } }; } else { 
                                                  0; }; } ;
 } 
void ** f38(void ** x1, void ** x23){  
((int) (1)) + ((int) (((fp) ((void **) ((void **) x1[0])[0])) (((void **) ((void **) x1[0])[1]), ((void **) x1[1])))) ;
 } void ** f37(void ** x1, void ** x22){  0 ; } 
void ** f2(void ** x1, void ** x3){  x1 ; } 

int main(){ 
 void** x33 = (void * []) {  }; 
 void** length = (void * []) { f34, x33 }; 
 ((fp) ((void **) length[0])) (((void **) length[1]), (void * []) { 1, 1, 
                                                                    (void * []) { 
                                                                    1, 2, 
                                                                    (void * []) { 
                                                                    0 } } }); 
}
