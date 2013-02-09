typedef void** (*fp)(void**,void**);


void ** f30(void ** x1, void ** z);
void ** f34(void ** x1, void ** x10);
void ** f33(void ** x1, void ** x9);
void ** f31(void ** x1, void ** x32);
void ** f25(void ** x1, void ** z);
void ** f29(void ** x1, void ** x12);
void ** f28(void ** x1, void ** x11);
void ** f26(void ** x1, void ** x27);void ** f30(void ** x1, void ** z){  
if (((int) (((int) (0)) - ((int) (((void **) z[0]))))) + ((int) (0)) == 0){ 
  void** x = ((void **) z[1]); 
  (void * []) { f33, (void * []) { x } }; } else { if (0 == 0){ 
                                                   void** a = z; 
                                                   (void * []) { f31, 
                                                                 (void * []) { 
                                                                 f34, 
                                                                 (void * []) { 
                                                                  } } }; } else { 
                                                   0; }; } ;
 } void ** f34(void ** x1, void ** x10){  3 ; } 
void ** f33(void ** x1, void ** x9){  ((void **) x1[0]) ; } 
void ** f31(void ** x1, void ** x32){  x1 ; } 
void ** f25(void ** x1, void ** z){  
if (((int) (((int) (0)) - ((int) (((void **) z[0]))))) + ((int) (0)) == 0){ 
  void** x = ((void **) z[1]); 
  (void * []) { f28, (void * []) { x } }; } else { if (0 == 0){ 
                                                   void** a = z; 
                                                   (void * []) { f26, 
                                                                 (void * []) { 
                                                                 f29, 
                                                                 (void * []) { 
                                                                  } } }; } else { 
                                                   0; }; } ;
 } void ** f29(void ** x1, void ** x12){  2 ; } 
void ** f28(void ** x1, void ** x11){  ((void **) x1[0]) ; } 
void ** f26(void ** x1, void ** x27){  x1 ; } 

int main(){ 
 void** x = (void * []) { f25, (void * []) {  } }; 
 void** y = (void * []) { f30, (void * []) {  } }; 
 ((fp) ((void **) x[0])) (((void **) x[1]), (void * []) { 0, 2 }); 
}
