module Main where
import Monad (liftM, when)
import C2HS

main = do
        let sz1 = {# sizeof S1 #}
        sz1expect <- liftM cIntConv $ {# call size_of_s1 #}
        when (sz1 /= sz1expect) $ error "Fatal: sizeof s1 != size_of_s1()"
        let sz2 = {# sizeof S2 #}
        sz2expect <- liftM cIntConv $ {# call size_of_s2 #}
        when (sz2 /= sz2expect) $ error "Fatal: sizeof s2 != size_of_s2()"
        let sz3 = {# sizeof S3 #}
        sz3expect <- liftM cIntConv $ {# call size_of_s3 #}
        when (sz3 /= sz3expect) $ error $ "Fatal: sizeof s3 != size_of_s3(): " ++ show sz3 ++ " but expected " ++ show sz3expect
        putStrLn (show sz1 ++ " & " ++ show sz2 ++ " & " ++ show sz3)
