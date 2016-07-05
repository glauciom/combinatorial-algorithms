package br.com.gm2.routines.java.next;

import org.junit.Assert;
import org.junit.Test;

import br.com.gm2.routines.java.next.NextPermutation;

public class NextPermutationTest {

	@Test
	public void testIdentityPermutation() {
		int n = 6;
		NextPermutation permutation = new NextPermutation(n);
		int[] result = permutation.actual();
		int[] expected = { 0, 1, 2, 3, 4, 5 };
		Assert.assertArrayEquals(result, expected);
	}

	@Test
	public void testIdentityOffSet() {
		int n = 6;
		NextPermutation permutation = new NextPermutation(n);
		int[] result = permutation.offset();
		int[] expected = { 0, 0, 0, 0, 0 };
		Assert.assertArrayEquals(result, expected);
	}

	@Test
	public void testLastEvenPermutation() {
		int n = 6;
		NextPermutation permutation = new NextPermutation(n);
		permutation.allPermutationsNoOutput();
		int[] result = permutation.actual();
		int[] expected = { 1, 2, 3, 4, 5, 0 };
		Assert.assertArrayEquals(result, expected);
	}

	@Test
	public void testLastEvenOffSet() {
		int n = 6;
		NextPermutation permutation = new NextPermutation(n);
		permutation.allPermutationsNoOutput();
		int[] result = permutation.offset();
		int[] expected = { 0, 0, 0, 0, n - 1 };
		Assert.assertArrayEquals(result, expected);
	}
	
	@Test
	public void testLastOddOffSet() {
		int n = 7;
		NextPermutation permutation = new NextPermutation(n);
		permutation.allPermutationsNoOutput();
		int[] result = permutation.offset();
		int[] expected = { 0, 0, 0, 0, n - 2, n - 1 };
		Assert.assertArrayEquals(result, expected);
	}
	
	@Test
	public void testLastOddPermutation() {
		int n = 7;
		NextPermutation permutation = new NextPermutation(n);
		permutation.allPermutationsNoOutput();
		int[] result = permutation.actual();
		int[] expected = {2,3,4,5,6,1,0};
		Assert.assertArrayEquals(result, expected);
	}
	
	@Test
	public void testValidateAfterLastPermutation() {
		int n = 7;
		NextPermutation permutation = new NextPermutation(n);
		permutation.allPermutationsNoOutput();
		int[] result = permutation.actual();
		int[] afterResult = permutation.nextper();
		Assert.assertArrayEquals(result, afterResult);
	}

	@Test
	public void testMinValue() {
		NextPermutation permutation = null;
		try {
			int n = 1;
			permutation = new NextPermutation(n);
			permutation.allPermutationsNoOutput();
			Assert.fail("check N must raise an Exception");
		} catch (IndexOutOfBoundsException e) {
			Assert.assertTrue(true);
		}
	}

}
