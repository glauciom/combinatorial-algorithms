package kNSubset;

/**
 * @author Glaucio Melo
 * Classe responsavel pelo processamento do algoritmo FullKNSubset.
 */
public class FullKNSubset {

	// Campos da classe FullKNSubset.
	
	private int i,n,p,x[],y[];
	private StringBuffer k1;
	
	
	/**
	 * Construtor de FullKNSubset. C(n,p)
	 * @param n Numero do conjunto.
	 * @param p Numero das partes.
	 */
	public FullKNSubset(int n, int p) {
 	 this.n = n;
 	 this.p = p;
 	 x = new int[p];
 	 y = new int[p+1];
	}

	// Metodo nucleo do algoritmo combiner,
	private int sum(int k, int j, int z) {
 	 for(i=0;i <= j;i++)
  	  z += y[i];
	return z+k;
	}

	// Metodo nucleo da classe FullKNSubset.
	private void combiner(int i) {
 	 for(x[i]=sum(i,i,0);x[i]<=n-(p-i);x[i]++)
  	  if (i != p-1) 
   	   combiner(i+1); 
  	  else 
   	   show();
 	  y[i+1]=0;
 	  y[i]++;
	}
	
	/**
	 * Invoca o metodo de combinatoria.
	 */
	public void combinatoriaAlgorithm() {
		k1 = new StringBuffer();
		combiner(0);
	}

	// Coloca o vetor de combinacao em uma string.
	private String show() {
	 for(i=0; i < p;i++)
	  k1.append(x[i]+" ");
	 k1.append("\n");
	 return k1.toString();
	}
	
	/**
	 * Metodo que captura o conjunto de Combinacoes n elementos p a p, em forma
	 * de String.
	 * @return String Conjunto de Combinacoes, em forma de String.
	 */
	public String getOutPut() {
		return k1.toString();
	}
		
	/**
	 * Metodo main (para testes).
	 * @param args Entrada de string de comando.
	 */
	public static void main(String args[]) {
		long r, r1;
		FullKNSubset c = new FullKNSubset(5,3);
		r = System.currentTimeMillis();
		c.combinatoriaAlgorithm();
		System.out.println(c.getOutPut());
		
		 /*LexicographicNextKSubset test  = 
		 	new LexicographicNextKSubset(50,8);
		 //System.out.println(test.getOutPut());
		 //test.seialLexicographicNextKSubset(1);
		// System.out.println(test.getOutPut());  
		 r = System.currentTimeMillis();
		 while (!test.isLastElement()) {
		  test.lexicographicNextKSubsetAlgorithm();
		 }
		 r1 = System.currentTimeMillis();
		 System.out.println("Tempo = "+(r1-r));
		 System.out.println(test.getOutPut()); */
		 
		
		
	}	
}