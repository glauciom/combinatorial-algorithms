package partition;

import partition.NextPartitionNSet;
import numbers.BellNumbers;

/**
 * Esta classe é responsável pelo processamento do algoritmo de partição serial
 * para n conjuntos.
 * 
 * @author Glaucio
 */

public class SerialPartitionNSetMethod {

	private int partition[];

	private int[][] d;

	private int n, i = 0, j = 0, result;

	private BellNumbers b;

	public SerialPartitionNSetMethod(int n) {
		this.n = n;
		partition = new int[n];
		b = new BellNumbers();
		result = 0;
		d = fillBuffer();
	}

	private int[][] fillBuffer() {
		int d[][] = new int[n][];
		for (int i = 0; i < n; i++) {
			d[i] = new int[n - i];
			d[i][0] = (int) b.B(n - i);
		}

		for (int i = 1; i < n; i++) {
			for (int j = 0; j < n-i; j++) {
				d[j][i] = d[j][i - 1] - i * d[j + 1][i - 1];
			}
		}
		
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n-i; j++) {
				System.out.print(d[i][j] + " ");
			}
			System.out.println();
		}	
		System.out.println();
		return d;
	}

	public int[] algorithm(int serial) {
		i = j = result = 0;
		for (int k = 0; k < n; k++) {
			partition[k] = element(serial);
		}
		return partition;
	}

	private int element(int serial) {
		int des = d[i][j];
		for (int t = 0; t < j + 1; t++) {
			if (result + des >= serial) {
				i++;
				return t;
			} else
				System.out.println(des + " - " + i + "," + j);
				result += des;
		}
		System.out.println();
		j++;
		return j;
	}

	public int[] getPartition() {
		return partition;
	}

	public String toString() {
		StringBuffer k1 = new StringBuffer();
		for (int i = 0; i < n; i++)
			k1.append(partition[i] + " ");
		return k1.toString();
	}

	/**
	 * Método main (para testes)
	 * 
	 * @param args
	 *            Parâmetros de entrada ao método.
	 */
	public static void main(String[] args) {
		prova();
	}

	public static void prova() {

		int n = 4;
		SerialPartitionNSetMethod test = new SerialPartitionNSetMethod(n);
		NextPartitionNSet test1 = new NextPartitionNSet(n);
		BellNumbers b = new BellNumbers();
		for (int h = 1; h <= b.B(n); h++) {
			System.out.println("serial = "+(h-1));
			int[] result = test.algorithm(h);
	//		test1.algorithm();
		//		int[] q = test1.getIndexes();
				//System.out.println(h);
		//		System.out.println(test);
		//		for (int t = 0; t < result.length; t++) {
		//			if(result[t] != q[t]) {
		//				System.out.println("Resultado Correto: "+test1.getOutput());
		//				System.out.println("Resultado do algoritmo: "+test);
		//				System.out.println("Falhou em: "+h);
			//			System.out.println();
			///			break;
		//			}
			//	}
		}
	}
}