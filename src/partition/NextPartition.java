package partition;

/**
 * @author Glaucio Melo
 * Classe responsavel pelo processamento do algoritmo de proxima particao
 * (NextPartition).
 */
public class NextPartition {

	// Campos da classe NextPartition.
	
	private int[] r,partition;
	private int i,n,s,m,q,k,f;

	/**
	 * Construtor para a classe NextPartition.
	 * @param n Numero a ser partitionado.
	 */
	public NextPartition(int n) {
 	 this.n = n;
 	 r = new int[n];
	}

	// Captura o ultimo elemento do vetor da particao.
	
	private int getLastElement() {
 	 for(i = 0; i < n-1; i++)
  	  if (r[i+1] == 0)
   	   return i;
	return n-1;
	}
	
	// Transforma o vetor total da particao em um vetor ajustado.
	
	private int[] transformPartition(int f) {
	 partition = new int[f+1];
	 for(i = 0; i <= f; i++)
	  partition[i] = r[i];
	 return partition;
	}
	
	// Metodo nucleo da classe
	
	private int[] algorithm() {
	 if (r[0] == 0) {
	  r[0] = n;
 	  return transformPartition(0);
 	 } 
 	 else {
 	  if (r[n-1] == 0) {
  	   while (r[k] == 1)
   	    k--;
   	   s = r[k] + f-k;
   	   m = r[k] -1;
   	   q = (int) Math.floor(s/m);
   	   for(i = k; (r[i] != 0) && i < n; i++)
	    r[i] = 0;
   	   for(i = 0; i < q; i++)
	    r[i+k] = m;
   	   if (s % m != 0)
	    r[i+k] = s % m;
   	   f = getLastElement();
   	   k = f;
	   return transformPartition(f);
 	  }
 	 }
 	 return null;
	}
	
	/**
	 * Captura o vetor de particao, em forma de String.
	 * @return Vetor de particao, sob forma de String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(i = 0; i < partition.length; i++)
	  k1.append(partition[i]+ " ");
	 return k1.toString();
	}
	
	/**
	 * Invoca o algoritmo de Proxima Particao.
	 */
	public void nextPartitionAlgorithm() {
		partition = algorithm();
	} 
	
	/**
	 * Captura o vetor de Particao.
	 * @return int[] Vetor de Particao.
	 */
	public int[] getPartition() {
		return partition;
	}
	
	/**
	 * Captura todas as particoes, em forma de String.
	 * @return String Conjunto total de particoes.
	 */
	public String getFullPartition() {
	 StringBuffer k1 = new StringBuffer();
	 nextPartitionAlgorithm();
	 int i =1;
	 while (partition != null) {
	  k1.append(i+" -> "+getOutPut() + "\n");
	  nextPartitionAlgorithm();
	  i++;
	 }
	 return k1.toString();
	}
	
	/**
	 * Captura a particao, dado um serial.
	 * @param serial Numero da particao desejada.
	 */
	public void serialNextPartition(int serial) {
	 r = new int[n];
	 for(int g = 1; g <=serial; g++) 
	  nextPartitionAlgorithm();
	}

	/**
 	 * Metodo main (para testes)
 	 * @param args String de entrada de dados.
 	 */
	public static void main(String args[]) {
	 NextPartition nexpar;
	 for(int i = 4; i < 5; i++) {
	  System.out.println("Partição de "+i+":");
	  nexpar = new NextPartition(i);
	  System.out.println(nexpar.getFullPartition());
	 }
	}
}