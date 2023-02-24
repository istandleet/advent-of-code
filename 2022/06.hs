{-# language OverloadedStrings #-}
{-# language BangPatterns #-}
{-# language LambdaCase #-} 
{-# language TupleSections #-}
{-# language TransformListComp #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Bifunctor
import Data.Either
import Data.Function
import Data.Maybe
import Data.Semigroup
import Data.Tuple
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as VM
import qualified Data.List
import Data.Array.Unboxed
import GHC.Exts (the, groupWith)
import Data.Time

-- solved by ChatGPT

main :: IO ()
main = do
    print $ findMarker input
    print $ findMarker' input

findMarker :: String -> Int
findMarker s = go s 0
  where
    go :: String -> Int -> Int
    go "" _ = 0
    go (x:xs) n
      | length xs < 3 = 0
      | allDifferent (take 4 (x:xs)) = n + 4
      | otherwise = go xs (n + 1)

allDifferent :: String -> Bool
allDifferent (x:xs) = x `notElem` xs && allDifferent xs
allDifferent _ = True

input = "lrdrgrcggjrgrddfqfqrqppptrtssnqqqlzqqslspppsddfnnwgnnwhhjttnsnswnnbqbrrcqrqdqcdddlnddwwlmmlmtmwmggjgjmmmrppqhhswhwhmhchtchhsttvbtvvpspjjqrrgqrqmqvmvzmvzzrnnqlnlwlttjstjtjvjddrzzrnrmrbmbwwzppnqpqmmjnnvqnvvdzdpdvvmhvhhmshmsssvnnqppclcqlcqctqtfffvrfvrffptfftppncncwnntfntftdftfhfzzfwzzcddngdndccrttjmmcddrvrwwnntggghccjbjvbvjvlvjlvlgljlnjjtvjjfddmwwhnnqnrnccwllpmprmmspsvsrsslstsllpmllfzlflqltlssdbbdsdqsdqqgfgfjjtzjzmmsnsmnnzssgfgzgjgwwqfqtfthhhczzpwprwwqttqcqctcpcrrvwrvrggfvfqvvbqqhnnczcjjjfbfhbhjhssnwwfswszwwgbgzgczcbbvmvrrdgrddfwdffvsvtstztftfvfssgffnrnnzzqddltdtbdttnnfrnnfrfddpmpqqplqqbdbzbnnnrttjfttprtrllqhhwjhjghjjcrcqqznztzllldjjhqqspsfpfprrsfspsrrzbbjmjssnjnsjnjllbttgrtrnnqhhvjhvvrrplrprtrtprtrdtdfttwdttqtttwvtvtmtntnnfvfsfnndsnnnfqfmmcdmdppsmpmnnwhhlwwrvwrwvwlwnwhwjwvwbvwvnnqtnnttcgtccbggzgwzwczcpzcpzpzgzngnzzhfhdhzzzsggtssddgtdtndtdllpbpjpgjjsllvqvmmvjmvjvbbfzzbdzzghhzjzvvnffwsshqhqhjhmmtgmgpmgmpmzzzhtzzbzsbzsbzszvsvgsvspvsstwtgwgqgjggsfsmffhlhmhdmhmgmrmffjcjggqgsghgttjffprpmmscsggswssgzzsqzqjqwqjwqwswjjqpjpllwrwffvfbfpbffjsswgsshjsjqssqrrvcrrnvvdnnwndnffpdffjlffrjrlrqrmmlzlttpccgchggdjjhwwvqvvrprdrttlvvtlvtlvvvhqhwhmmpmrrjsrrzbzfzdzhdzzwgwwqllmmthmmvpprqprrzmrrfrmfrrmfmjmwwhtwhttnbbhbnnhbnnmhmgmjgmgfmmhpplwplwlswwjjbpbhpbbzmmgjmmggbzbssppjtttfmfwwcwmmbmrmbrblbmmszzvgvzgvgvdggjgsjgsjgghfgfngffsnfssdpdgpgrgnrggrlggdwgdgddfwddjdrrppnnqbnntmmrmlmrrgvvqjjwzzgnzzdtzzbccnbcbttbzzcqqrcqqpspbphbphhnpnccggczcgcpgcpctppqwpwccsgsttgtbttvftfcfbfvfppfpnpffmjmljmmrtrsrttnhhcvctclttrffhzhrzzdpzzwvwddcggpbptbpbzpzdpzddbjjdvvwmmfvfcfmfmmvrmvrmmcdmdqqhjqhqmmstmmdnnvssmbsmmnhmnmlmttlthtvhvvgbvgvbvqbvbvnbvvpwwhwsszpszppjljsllzhzcchmhdhphvvtpvvfssvmvcmmrjmmgfmfwmfmsspbbpnnsbsgbbzzllzzsqqswqqsbqqlvqlqzzbrzzqsqdqjqzqlqccqbqlblpprrthrttjzzfvzzpvpbppqjpjjtllmrmzzwnnrggpfpllwhhnmnpmmdppwnnwpnnzlzggvpvgpprdrzddlmddjndjjrljlsjshhqbbhhmhvhqhzqqlbqlqnqqmhmwmhwwgbwbpbhpbblnnshnshslllrcrncctnthtllmglmlmnmfnfllrbrmdqsncjfbjnghmvpctfzttqwsfptwhfhmdhzgbmrpqvsbbwdwdnqgclrgvdbqvtnzvlzmdjzgbhbtqjhrgqqjgwcvpqhhwsmmflgsjsvbnjcjcqwqqfcbvfrllbpphfglptjfczmnfnrmqdgrhbvrddwvhnbffsnzntvldrwmgqmqdbmmpdjhpdqfsbzsfbtdzlfjwtwfpfrhzcwwrcfpdjzjzqvcnhvdvlbdbjfrnmchdjprbfpzdtmjhdjtrwfrsghngbswhfzdfvhdmnqwnpspdjgsfcjjhrlnpncfdhzfzglgvrjlzdcncbfjhpnrvwnqbzhtdfcnpnpddsnjcbgdvzlsmpnlbftgjfsjsslljppjhtjfddmwbtrvmhvshnvhhfdsqhpmgzfrcqtpdmfcwglnbjzmtmzshzsrnbzlwlzdjzzsqfmjzbnprjdzswwcgjjwslhnfhbflvcpdfzwhzzjmfvpfhzbrcdvtlmbvrqvvtlpbdnshtsvlcnlwqzcnfhzndfjbhgwpvspdvhfptnfvwznscmgthspwpjfwlbbcrzgthrhnvscvfvwrwvtzpjqmwdbjjslvzlmqstzwqtsdsqqjpqthgnshpjncwgvppngvvfrjpztftgjbbmgdddrcgjggwpfrvfqbnzvwtscftbwfssmrgrbchjmvqcdgbdmmsswvplnvnjrnqzhttbzhlthzjslgtgrnqpghtbjnbftpdttvqhrljhnfqllrhfmzcnnwrljmchsvdbgvwmrpfzjsbngpffpgntrgflntbjnjwqbdhvhssvptdvvqvtrgqhhzrcfnnmbtzqrcghggcqhndggqzzgswflhqqrgmnggbrvpwqgtlptgmzdvgztgfqdzwnbrvvqhpnmcmptqljmqqmsslfcmgpnmnjmrsngrtbsffqbwlhsrwbzdcqvpbptpjphcjnwsmrjdbbrwftsnrgfhpjvsjcwpmpvfjtjvfnnwdjdttsjrqrgsznzqwjmsvscdtpptmdhqvwngtldtsnstjmwwrwwzdzvtndrrhgsgshzlpdrmlsgvplvbfrffvgvhmncbjdlqspbpdpcdsffrjzptltsznwwqbvnrwfdtrlbbvsmzjrhhvscqfwsqtmwnpjfgnjcttwphtqqhnvgjmzvczcrlmjjgwgnprcmmprltfrgrjlhvggdwvpnrqrtfwhpsfnjfvmvgzpmhrqmggldmsvztwcwzgblfbvbfvnshrdppzcvjwbjmsncwnbnjwbmwqjjwtjmwzpdvpwrfdtrqhnltgntvglgspvnbcsvnpppwmgphhsdqtpmrzmbdqlghsgjnnbhflzwghzzhdfsvjjcjfcssmvrmfqbgzcfwmhpvjqqrrhpsffczwgbjgwqlvzrvnhvzrqnfllqtrcjhmpdjfpqnlzhdnsctbfhsbpgwqdjdjhfbqzvzpsgmqwfjsffdjcqfmjgzqzvfsbhhvnwlfjfmdnphggdbnmpznlrzbnlbvhvplhjbzdmspnnlbctgzphghpngppdpdfbcdcpfntqlrwclsvnpljdbwcbhwzzdhbhsmslvvtjsmqmtpnhmmqnhfggnlpfthdhpwmrhzgfpwsffnrdqszcttrjsqzjqgspltfzzjnzwdfzvnmncwnphmjvcwlgrzcvpzcbndvhtjnfsgjtjdqfmgcgtgppsrcqwdjcqfddlhhnlfjrnsnssqblmnjjvmfbghtwwgcpgzfddvzsrsghqfrpvdtqmjfrzpvclmsnpmrqngdwvznlhdpbrzqtswfhnblnnbwjcwwbwfrgcdgrpphnslgnwbtfcgcmvvllwgvrcrdfcfwvwmdhhzmmnzmjgmgrgpdhngjgmhlqwtgzlngrbbfwfjrpwbqjnvdggrcfdgphfctvbmjnwfbpwrvbdbjrbhtnhfvhwvrptptsdrnzmnlwgbrwcswrccwdftgvjnvhffghdvhltjwhppfwfnmmtwclzzftzwvmhpmgvdsqzrfwqsmcgswnzjcnrvdgjlqdjrczphsvldlfzwdwmpncpvgqsvgpfpsfbgbmdhnfqbhdqwwwfdgqtmjlfbztsrwjrtqnrfpfqgplznpftrnjzhzcrnngqpwbrpbhlbfsgrpwfflrpbqdrqdplgcn"

findMarker' :: String -> Int
findMarker' s = go s 0
  where
    go :: String -> Int -> Int
    go "" _ = 0
    go (x:xs) n
      | length xs < 13 = 0
      | allDifferent' (take 14 (x:xs)) = n + 14
      | otherwise = go xs (n + 1)

allDifferent' :: String -> Bool
allDifferent' s
  | length s < 14 = False
  | otherwise = go s
  where
    go :: String -> Bool
    go (x:xs) = x `notElem` xs && go xs
    go _ = True