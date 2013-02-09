#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

#ifdef __MACH__
	#include <mach/clock.h>
	#include <mach/mach.h>
#endif

void print_cmd(char * cmd){
   printf("-(> Executed from shell : %s\n",cmd);
}

void exec_cmd(char * cmd){
   print_cmd(cmd);
   system(cmd);
}

char* concatenate(char * c1, char * c2){
  char * newchar = calloc(strlen(c1)+strlen(c2)+1, 0x00);
  int i = 0, j = 0;
    
printf("<%s> <%s>\n",c1,c2);
printf("(%u) (%u)\n",strlen(c1),strlen(c2));
  

  for(i = 0; i < strlen(c1); i++)
	*(newchar+i) = *(c1+i);
	
printf("{%s}\n",newchar);
	
  for(j = 0; j < strlen(c2) && i < strlen(c1)+strlen(c2); j++){
    *(newchar+i) = *(c2+j)
    i++;
  }
  
printf("<%s>\n",newchar);
printf("(%u)\n",strlen(newchar));
  	
  return newchar;
}

char* chop_extension(char * name){
   int i = strlen(name) - 1;
   for(i; i >= 0; i--){
      if(name[i] == '.') break;
   }
   if(i == 0) return name;

   char * newchar = malloc(sizeof(char*) * (i)+1);
   int j = 0;
   for(j; j < i; j++){
	newchar[j] = name[j];
   }   
   newchar[i] = 0;
   return newchar;
}

char* get_filename(char * name){
   int i = strlen(name) - 1;
   for(i; i >= 0; i--){
      if(name[i] == '/') break;
   }
   if(i == 0) return name;

   char * newchar = calloc(sizeof(char*) * (strlen(name) - 1 - i), 0x00);
   int j = 0;
   for(j; j < (strlen(name) - 1 - i); j++){
	newchar[j] = name[i+j+1];
   }   
   return newchar;
}

long getnanotime(){
   struct timespec tp;
   
   #ifdef __MACH__ // OS X does not have clock_gettime, use clock_get_time
		clock_serv_t cclock;
		mach_timespec_t mts;
		host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
		clock_get_time(cclock, &mts);
		mach_port_deallocate(mach_task_self(), cclock);
		tp.tv_sec = mts.tv_sec;
		tp.tv_nsec = mts.tv_nsec;
   #else
   
   	clock_gettime(CLOCK_REALTIME, &tp);
   
   #endif
   
   return tp.tv_nsec;
}


// OCAML ACTIONS 
// =============
// IGNORE
// sh ./compileToCaml.sh
// TIMED
// ./toCaml.native -notrace -compile %filename

unsigned long mesureOcaml(char * filename){
   exec_cmd("sh ./compileToCaml.sh");

   char * cmd = concatenate("./toCaml.native -samename -compile ",filename);

   long time1 = getnanotime();
   exec_cmd(cmd);
   time1 = getnanotime() - time1;
   
   free(cmd);

   return (time1);
}

unsigned long mesureOcaml2(char * filename){
   char * ocamlName = concatenate(get_filename (chop_extension (filename)),".ml");	
   char * fullPath  = concatenate("./translated/",ocamlName);

   char * cmd = concatenate("echo \";;\" >> ", fullPath);
   exec_cmd(cmd);
   
   free(cmd);
   cmd = concatenate("cat ", fullPath);
   char * cmd2 = concatenate(cmd," | ocaml");

   long time1 = getnanotime();
   exec_cmd(cmd2);
   time1 = getnanotime() - time1;
   
   free(ocamlName); free(fullPath); free(cmd); free(cmd2);

   return (time1);
}

// C ACTIONS
// =========
// IGNORE
// make
// TIMED
// ./ted --compile --target willow --hamlet %filename
// IGNORE 
// mv filename (tronqué) ^ .compiled-willow
// sh ./compileToC.sh
// TIMED
// ./toC.native -samename -compile %filename

unsigned long mesureC(char * filename){
   long total_time = 0l;
   long templong; 

   char * ted_cmd = concatenate("./ted --compile --target willow --hamlet ",filename);
   char * ted_cmd2 = concatenate(ted_cmd," 2> /dev/null");
   char * willow_name = concatenate(get_filename (chop_extension (filename)),".compiled-willow");
   char * new_willow_name = concatenate (" ./translated/",willow_name);
   char * mv_cmd  = concatenate("mv -f ",willow_name);
   char * mv_cmd2 = concatenate(mv_cmd, new_willow_name);
   char * rm_cmd  = concatenate("rm -f ",willow_name);
   char * toC_cmd = concatenate("./toC.native -samename -compile",new_willow_name);

   exec_cmd("make");

   templong = getnanotime();
   exec_cmd(ted_cmd2);
   total_time += (getnanotime() - templong);
   
   exec_cmd(mv_cmd2);
   exec_cmd("sh ./compileToC.sh");

   templong = getnanotime();
   exec_cmd(toC_cmd);
   total_time += (getnanotime() - templong);
  
   exec_cmd(rm_cmd);

   free(ted_cmd); free(ted_cmd2); free(willow_name); free(new_willow_name); free(mv_cmd); free(mv_cmd2); free(rm_cmd); free(toC_cmd);

   return total_time;
}

unsigned long mesureC2(char * filename){
   char * cName     = concatenate(get_filename (chop_extension (filename)),".c");	
   char * fullPath  = concatenate("./translated/",cName);
   char * execName  = concatenate(get_filename (chop_extension (filename)),".out");
   char * execPath  = concatenate("./translated/",execName);

   char cmd[100];
   sprintf(cmd,"cc -o %s %s",execPath,fullPath);
   
   exec_cmd(cmd);

   long time1 = getnanotime();
   exec_cmd(execPath);
   time1 = getnanotime() - time1;
   
   free(cName); free(fullPath); free(execName); free(execPath);

   return (time1);
}


int main(int argc, char** argv){
   
   printf("\n");
   if(argc < 2){
	printf("Analyse de performances de TED.\n");
	printf("===============================\n");
	printf("Veuillez spécifier un argument.\n");
	printf("[ Fichier hamlet à compiler ]  \n");
	printf("\n");
	exit(1);
   }
   char * filename = argv[1];

   printf("\n   MESURE OCAML   ");
   printf("\n------------------\n");
   unsigned long camlTime  = mesureOcaml (filename);
   unsigned long camlTime2 = mesureOcaml2(filename);
   printf("\n------------------\n");
   printf("\n   MESURE   C     ");
   printf("\n------------------\n");
   unsigned long cTime     = mesureC (filename);
   unsigned long cTime2    = mesureC2(filename);
   printf("\n------------------\n");

   char * fastest = "Vers C";
   if(camlTime < cTime) fastest = "Vers OCaml";
   unsigned long ecart = camlTime - cTime;
   if(camlTime < cTime) ecart = -ecart;

   printf("\n\n");
   printf("================================\n");
   printf("===   RESULTATS EN NANOSEC   ===\n");
   printf("================================\n");
   printf("=  Temps de compilation Ocaml  =\n");
   printf("   [%lu]                        \n",camlTime);
   printf("=  Temps de compilation C      =\n");
   printf("   [%lu]                        \n",cTime);
   printf("=  Ecart et plus rapide        =\n");
   printf("   [%lu] [%s]                   \n",ecart,fastest);
   printf("================================\n");
   printf("=  Temps d'exécution Ocaml     =\n");
   printf("   [%lu]                        \n",camlTime2);
   printf("=  Temps d'exécution C         =\n");
   printf("   [%lu]                        \n",cTime2);
   printf("================================\n");
   printf("\n");

   return 0; 
}
