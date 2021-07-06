{-# LANGUAGE TemplateHaskell #-}
module User where

import           Data.Text
import           Lens.Micro.Platform


data User = MkUser
	{ _firstName :: Text
	, _lastName  :: Text
	, _address   :: Address
	}
	deriving (Eq, Show)

data Address = MkAddress
	{ _zipCode :: Int
	, _street :: Text
	}
	deriving (Eq, Show)


makeLenses 'MkAddress
makeLenses 'MkUser

address1 :: Address
address1 = MkAddress { _zipCode = 123, _street = pack "foobarStreet" }

user1 :: User
user1 = MkUser { _firstName = pack "Joona", _lastName = pack "Piirainen", _address = address1 }

testView :: Int
testView = user1 ^. (address . zipCode)

updateZipCode :: User -> Int -> User
updateZipCode user newZip = user & (address . zipCode) .~ newZip


{-|
>>> testView
123

>>> updateZipCode user1 69
MkUser {_firstName = "Joona", _lastName = "Piirainen", _address = MkAddress {_zipCode = 69, _street = "foobarStreet"}}
-}
