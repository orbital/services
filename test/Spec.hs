import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map
import Control.Config (loadEnvFile, loadEnvSystem, loadEnv)
import System.Environment (setEnv)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ testGroup "Config"
        [ testCase "file" testFile
        , testCase "system" testEnv
        , testCase "defaults" testDefaults
        ]
    ]

testFile :: Assertion
testFile = do
    e <- loadEnvFile "test/test.env"
    Map.lookup "WOOT" e @?= Just "woot"
    Map.lookup "HELLO" e @?= Just "hello"
    Map.lookup "NUM" e @?= Just "1234"
    Map.lookup "URL" e @?= Just "http://www.test.com/lookupServlet1?lookupServiceName=AccessPoint&lookupServiceVersion=1.0&serviceName=NetConnectDemo&serviceVersion=2.0&responseType=text/plain"

testEnv :: Assertion
testEnv = do
    setEnv "SERVICES_TEST" "services-test"
    e <- loadEnvSystem
    Map.lookup "SERVICES_TEST" e @?= Just "services-test"

testDefaults :: Assertion
testDefaults = do
    setEnv "SERVICES_TEST_DEFAULTS" "value"
    e <- loadEnv "test/test.env"
    print e
    Map.lookup "SERVICES_TEST_DEFAULTS" e @?= Just "value"
    Map.lookup "HELLO" e @?= Just "hello"


