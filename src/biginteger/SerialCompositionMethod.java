package biginteger;

import java.math.BigInteger;


/**
 * Esta classe é responsável pelo processamento do algoritmo de composição
 * serial, obtido através de análise ao algoritmo de próxima composição.
 * @author Glaucio
 */

public class SerialCompositionMethod {

    //Vetor de composição.
	private int composition[];
	
	//Valor a ser composto e o numero de partes em que ele vai ser composto.
	private int n, k;
	
	//Serial de entrada para o algoritmo.
//	private long serial;
	private BigInteger serial; 
	
	private BigInteger max;
	//Campos auxiliares ao método.
//	private long  aproximation, x, y;
	private BigInteger aproximation;
	private int i,j,g, complement,x, y;

	
	/**
	 * Construtor para a classe SerialCompositionMethod
	 * @param n Numero que irá ser composto.
	 * @param k Numero de partes em que n será composto.
	 */
	public SerialCompositionMethod(int n, int k) {
		this.n = n;
		this.k = k;
		composition = new int[k];
	}
	
	
	//Calcula o fatorial de um numero.
	private String factorial(int r, int op) {
		BigInteger aux = BigInteger.ONE;
		for (int t = r; t > op; t--) {
			aux = aux.multiply(BigInteger.valueOf((long) t));
		}
		return aux.toString();
	}
	
	
    //Calcula o binomial, com otimizações quanto à multiplicação. 
	private BigInteger getBinomialElements(int r, int s) {
		if ((r - s) < s)
			return new BigInteger(factorial(r, s)).divide(new BigInteger(factorial(r - s, 1)));
		return new BigInteger(factorial(r, r - s)).divide(new BigInteger(factorial(s, 1)));
	}
	
	
	//Método nucleo que captura o valor do elemento do vetor de composição. 
	private int getElement() {
		BigInteger aux = BigInteger.ZERO;
		for (j = 0; j < n; j++) {
			aux = aproximation.add(getBinomialElements(x - i - j, y - j));
			if (aux.compareTo(serial) <= 0)
				aproximation = aproximation.add(getBinomialElements(x - i - j, y - j));
			else {
				x = x - j;
				y = y - j;
				return j;
			}
		}
		return n;
	}


	//Método nucleo de invocação do algoritmo.
	private int[] serialCompositionAlgorithm(BigInteger serial) {
		complement = 0;
		aproximation = BigInteger.ZERO;
		x = n + k - 2;
		y = n;
		for (i = 0; i < k - 1; i++) {
			g = getElement();
			composition[(int) ((int)k - 1 - i)] = (int) g;
			if (composition[(int) ((int)k - 1 - i)] != 0)
				complement += composition[(int) (k - 1 - i)];
		}
		composition[0] = (int) (n - complement);
		return composition;
	}
	
	
	/**
	 * Método de invocação do algoritmo de composição serial.
	 * @param serial Serial passado como parâmetro.
	 */
	public void serialComposition(String serial) {
		this.serial = new BigInteger(serial).subtract(BigInteger.ONE);
		composition = serialCompositionAlgorithm(this.serial);
	}

	
	/**
	 * Método sobreposto que retorna como saída o vetor de composição, sob forma
	 * de String.
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer k1 = new StringBuffer();
		for (int i = 0; i < k; i++)
			k1.append(composition[i] + " ");
		return k1.toString();
	}
	
	/**
	 * Captura o número total de composições de um inteiro n em k partes. 
	 * @return int
	 */
	public String getNumberOfCompositions() {
		return getBinomialElements(n+k-1,n).toString();
	}
	
	public void calculateMax() {
		if (max == null) {
			max = new BigInteger(getNumberOfCompositions());
		}
	}
	
	public String getMax() {
		return max.toString();
	}


	public boolean isLastComposition() {
		return this.serial.compareTo(max) == 0;
	}
	
	
	/**
	 * Método main (para testes)
	 * @param args Parâmetros de entrada ao método.
	 */
	public static void main(String[] args) {
		SerialCompositionMethod test = new SerialCompositionMethod(90800,100);
		test.calculateMax();
		System.out.println(test.getMax());
		test.serialComposition("80197799290211588157845116201573300408801572711922298630661809179270346051499872152293612664510725931113829724381722483659251249351823280175326019892324964391746646556069538473357995814181406250299869445094520779759728694969732791611839004688546510907145290202777262839749895318198987889360768983034254317149368751");
		System.out.println(test);
	//	for(int i = 1; i <= test.getNumberOfCompositions(); i++) {
	//	 Date r = new Date();
	//	 long t0 = r.getTime();
	//	 test.serialComposition(""+i);
	//	 System.out.println(i + "-> "+ test);
//		}
	//    r = new Date();
	//    long t1 = r.getTime() - t0; 
	 //   System.out.println("tempo: "+ t1);
	}

	/**
	 * @return
	 */
	public int[] getComposition() {
		return composition;
	}

}
