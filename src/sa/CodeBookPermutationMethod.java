/*
 * Created on 18/09/2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package sa;

import java.io.File;

import permutation.NextPermutation;
/**
 * @author GLAUCIO Método de Busca para melhor configuração dos índices do
 *         dicionário --> Resta fazer a estratégia de busca nesta lista.
 */
public class CodeBookPermutationMethod {

	/**
	 * Objeto que representa o dicionario em arquivo.
	 */
	private CodeBook codebook;

	/**
	 * Representa os dados do dicionario.
	 */
	private int[][] data;

	/**
	 * Tamanho da palavra binaria.
	 */
	private int numberOfBits;

	/**
	 * Constante que define o Logaritmo de 2 na base natural.
	 */
	private final double LN2 = 0.69314718055994530941723212145818;

	/**
	 * Construtor para CodeBookPermutationMethod (Padrão Java Beans).
	 */
	public CodeBookPermutationMethod() {
		this.codebook = new CodeBook();
		this.data = codebook.getData();
		this.numberOfBits = 0;
	}

	/**
	 * Construtor para CodeBookPermutationMethod.
	 * 
	 * @param f
	 *            Nome do Arquivo do dicionário.
	 * @param numberOfVectors
	 *            Número de vetores do dicionário.
	 * @param dimension
	 *            Dimensao do vetor
	 */
	public CodeBookPermutationMethod(File f, int numberOfVectors, int dimension) {
		this.codebook = new CodeBook(f, numberOfVectors, dimension);
		this.data = codebook.getData();
		this.numberOfBits = (int) (Math.log(data.length) / LN2);
	//	System.out.println(numberOfBits);
	}
	
	
	/*public int[] algorithm(int n) {
		SerialPermutationMethod spm = new SerialPermutationMethod(n);
		String left = "1";
		String right = Utils.factorial(n,1);
		String center = Utils.divideBy2(right);
		int[] pLeft = null, pRight = null, pCenter = null;
		int min = 0;
		int ileft = -1;
		int iright = 0;
		int icenter = 1;
		int deltaLeft, deltaRight;
		for (int i = 0; i < 20000; i++) { //710
			spm.algorithm(left);
			pLeft = spm.getPermutationArray();
			spm.algorithm(right);
			pRight = spm.getPermutationArray();
			spm.algorithm(center);
			pCenter = spm.getPermutationArray();
			ileft = Ides(pLeft);
			iright = Ides(pRight);
			icenter = Ides(pCenter);
			min = min(ileft, icenter, iright);
			if(min == icenter && min == ileft && min == iright) {
				System.out.println("parou em: "+i);
				break;
			}
		/*	deltaLeft = Math.abs(icenter - ileft);
			deltaRight = Math.abs(iright - icenter);*/
		/*	System.out.println("deltaLeft = "+deltaLeft);
			System.out.println("DeltaRight = "+deltaRight);*/
		/*	if(ileft == min) {
				right = new String(center);
				center = new String(Utils.divideAndOffset(right,left));
			} else 
				if(ileft != min) {
				left = new String(center);
				center = new String(Utils.divideAndOffset(right, left));
			}
		}
		System.out.println("Center = "+Ides(pCenter));
		System.out.println("Left = "+Ides(pLeft));
		System.out.println("Right = "+Ides(pRight));
		System.out.println("Mínimo = "+min(Ides(pLeft), Ides(pCenter), Ides(pRight)));	
		for (int i = 0; i < pCenter.length; i++) {
			System.out.print(pCenter[i] + " ");
		}
		System.out.println();
		return pRight;
	}
	
	public int[] algorithmByIdes(int Ides, int n) {
		SerialPermutationMethod pe = new SerialPermutationMethod(n);
		pe.algorithm(""+Ides);
		String number = ""+Ides;
		int[] pArray = pe.getPermutationArray();
		int min = 1000000000;
		int result[] = null;
		for (int i = 0; i < 300; i++) {
			int Ides2 = Ides(pArray);
		//	System.out.println(number);
			if(Ides2 < Ides) {
				number = Utils.divideAndOffsetSub(number,number);
			} else {
				number = Utils.divideAndOffsetAlternative(number,number);
			}
			if(min > Ides2) {
				min = Ides;
				result = pArray;
			}
			pe.algorithm(number);
			pArray = pe.getPermutationArray();
			Ides = Ides2;
		}
		System.out.println("minimo = "+min);
		for (int i = 0; i < result.length; i++) {
			System.out.print(result[i] + " ");
		}
		return result;
	}*/
	
	
	public void examinarIdes(int n) {
		NextPermutation next = new NextPermutation(n);
		int ides = Integer.MAX_VALUE;
//		for (int i = 0; i < 100000; i++) {
		//	System.out.println(next.outPut(next.actual()));
			System.out.println(Ides(next.actual()));
			//System.out.println(Ides(next.actual()));
//			if(x < ides) {
//				ides = x;
//			}
//		}
//		System.out.println();
//		System.out.println(ides);
	}
	
	public int min(int left, int center, int right) {
		return Math.min(Math.min(left,center),right);
	}
	/**
	 * Índice de desordem do dicionario.
	 * 
	 * @param permutation
	 *            Arranjo do dicionario.
	 * @return valor de indice de desordem.
	 */
	public int Ides(int[] permutation) {
		int[] H = null;
		int result = 0;
		for (int i = 0; i < permutation.length; i++) {
			H = getBitSet(permutation[i], (int) numberOfBits);
			for (int j = 0; j < H.length; j++) {
	//			System.out.println(permutation[i]);
				result += DM2(data[i], data[H[j]]);
			}
		}
		return result;
	}

	/**
	 * Retorna o conjunto de Hamming que diferencia k em um bit.
	 * 
	 * @param k
	 *            indice atual do laço
	 * @param n
	 *            numero de bits da palavra binaria.
	 * @return vetor de indices em H^1.
	 */
	public int[] getBitSet(int k, int n) {
		int[] res = new int[n];
		int index = 0;
		for (int j = p(n - 1); j > 0; j = j / 2)
			res[index++] = ((k & j) == 0) ? k + j : k - j;

		return res;
	}

	/**
	 * Calcula || w1 - w2 ||^2
	 * 
	 * @param w1
	 *            primeiro vetor
	 * @param w2
	 *            segundo vetor
	 * @return distancia media quadratica.
	 */
	public int DM2(int[] w1, int[] w2) {
		int sum = 0, aux = 0, n = w2.length;
		for (int i = 0; i < n; i++) {
			aux = (w2[i] - w1[i]) * (w2[i] - w1[i]);
			sum += Math.sqrt(aux);
		}
		return sum;
	}

	/**
	 * Retorna a potência de 2 elevado a b.
	 * 
	 * @param b
	 *            indice da potencia
	 * @return 2^b.
	 */
	private int p(int b) {
		return (int) Math.pow(2, (int) b);
	}

	/**
	 * Metodo main (para testes).
	 * 
	 * @param args
	 *            String do prompt de comando.
	 */
	public static void main(String[] args) {
		int n = 32;
		CodeBookPermutationMethod pe = new CodeBookPermutationMethod(new File(
				"src\\codebooks\\airplane_boat_gull_goldhill_32.dic"), n,16);
	//	SerialPermutationMethod spm = new SerialPermutationMethod(128);
		int[] arr = new int[n];
		for (int i = 0; i < arr.length; i++) {
			arr[i] = i;
		}
		long l = System.currentTimeMillis();
		pe.examinarIdes(n);
		/*System.out.println(pe.Ides(arr));
		//int result[] = pe.algorithm(n);
	//	int result[] = pe.algorithmByIdes(pe.Ides(arr),n);
		System.out.println("Tempo = "+ (System.currentTimeMillis() - l));
	//	System.out.println("Delta = "+ (pe.Ides(arr) - pe.Ides(result)));*/
	}
}