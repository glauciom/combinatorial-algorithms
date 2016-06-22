package partition;

/**
 * @author Glaucio Melo.
 * Esta Classe prove o metodo de particao randomica.
 */
public class RandomPartition {
	
	// Campos da classe RandomPartition.
	private int m,n,d,j,k;
	private int p[];
	private int partition[];

	/**
	 * Construtor para a classe RandomPartition.
	 * @param n Numero inteiro a ser particionado.
	 */
	public RandomPartition(int n) {
		this.n = n;
	}
	
	
	//	Transforma o vetor total da particao em um vetor ajustado.
	
	 private int[] transformPartition(int f) {
	  partition = new int[f+1];
	  for(int i = 0; i <= f; i++)
	   partition[i] = p[i];
	  return partition;
	 }
	 
	/**
	 * Captura o vetor de particao, em forma de String.
	 * @return Vetor de particao, sob forma de String.
	 */
	public String getOutPut() {
	 StringBuffer k1 = new StringBuffer();
	 for(int i = 0; i < partition.length; i++)
	  k1.append(partition[i]+ " ");
	 return k1.toString();
	}	 
	
	//Metodo nucleo da classe.
	private int[] randomPartition() {
	 p = new int[n];
	 m = n;
	 k = 0;
	 while(m != 0) {
	 do {
	   d = (int) (Math.random() * n) + 1;
	   j = (int) (Math.random() * n) + 1;
	 } while((m - j * d) < 0);
	 for(int i = k; i < j+k; i++)
	  p[i] = d;
	 m = m -j*d;
	 k = k+j;
	}
	return transformPartition(k-1);
	}
	
	/**
	 * Invoca o metodo nucleo da classe.
	 */
	public void randomPartitionAlgorithm() {
		partition = randomPartition();
	}
	
	/**
	 * Captura a particao gerada randomicamente.
	 * @return int[] Vetor que representa a particao dinamica. 
	 */
	public int[] getPartition() {
		return partition;
	}
	
	/**
	 * Metodo main (para testes).
	 * @param args String de entrada.
	 */
	public static void main(String[] args) {
		RandomPartition test = new RandomPartition(6);
		for(int t = 0; t < 100; t++) {
			test.randomPartitionAlgorithm();
			System.out.println(test.getOutPut());
		}
	}
}