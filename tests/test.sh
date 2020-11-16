let COUNT=0
let PASSED=0
let FAILED=0

echo -e "\e[39m########TESING JVM########"
echo -e "\e[39m"

for i in $(ls ./tests/examples/*.ins)
do
	echo -e "\e[39mTesting $i for JVM"
	let COUNT++

	TEST_NAME=$(echo $i | cut -c18-23)
	./insc_jvm $i
	cd ./tests/examples
	java $TEST_NAME > tmp.output
	DIFFERENCE=$(diff tmp.output $TEST_NAME.output)
	if [[ $DIFFERENCE -eq "" ]] 
	then
		echo -e "\e[32mPASSED"
		let PASSED++
	else
		echo -e "\e[31mFAILED"
		let FAILED++
	fi
	cd ../..
done

rm ./tests/examples/*.j 
rm ./tests/examples/*.class

echo -e "\e[32mPassed $PASSED/$COUNT"
echo -e "\e[31mFailed $FAILED/$COUNT"
echo -e "\e[39m"
echo -e "\e[39m"

######################################################

let COUNT=0
let PASSED=0
let FAILED=0

echo -e "\e[39m########TESING LLVM########"
echo -e "\e[39m"

for i in $(ls ./tests/examples/*.ins)
do
	echo -e "\e[39mTesting $i for LLVM"
	let COUNT++

	TEST_NAME=$(echo $i | cut -c18-23)
	./insc_llvm $i
	cd ./tests/examples
	lli $TEST_NAME.bc > tmp.output
	DIFFERENCE=$(diff tmp.output $TEST_NAME.output)
	if [[ $DIFFERENCE -eq "" ]] 
	then
		echo -e "\e[32mPASSED"
		let PASSED++
	else
		echo -e "\e[31mFAILED"
		let FAILED++
	fi
	cd ../..
done

rm ./tests/examples/*.bc
rm ./tests/examples/*.ll

echo -e "\e[32mPassed $PASSED/$COUNT"
echo -e "\e[31mFailed $FAILED/$COUNT"