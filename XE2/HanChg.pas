{
  천리안 프포그램 동호회에서 받은 자료를 제가 조금 더 수정을 해보았습니다..
  컴파일이 안되서리..
  때문에 코드가 정확한지는 자신이 없으니 이해해주시기 바랍니다..
  아래는 원래 올리신 분의 글입니다..

 아래 채경삼님이 올려주신 C 코드를 파스칼코드로 변환한 것입니다.
 변환자: 뫼빛 이만준

 사용법
  1. uses 절에 HanChg 추가

  2. 조합형->완성형
     완성형문자열변수 := WanJoChg(조합형문자열변수, true);

  3. 완성형->조합형
     조합형문자열변수 := WanJoChg(완성형문자열변수, false);
}

unit HanChg;

interface

function WanJoChg(Src : string; p_mode : Boolean):string;

implementation

const
  //완성형코드에 대응하는 조합형 테이블
  KSTable : array[0..2349]  of Word = (
  $8861, $8862, $8865, $8868, $8869, $886a, $886b, $8871, $8873, $8874, $8875,
  $8876, $8877, $8878, $8879, $887b, $887c, $887d, $8881, $8882, $8885, $8889,
  $8891, $8893, $8895, $8896, $8897, $88a1, $88a2, $88a5, $88a9, $88b5, $88b7,
  $88c1, $88c5, $88c9, $88e1, $88e2, $88e5, $88e8, $88e9, $88eb, $88f1, $88f3,
  $88f5, $88f6, $88f7, $88f8, $88fb, $88fc, $88fd, $8941, $8945, $8949, $8951,
  $8953, $8955, $8956, $8957, $8961, $8962, $8963, $8965, $8968, $8969, $8971,
  $8973, $8975, $8976, $8977, $897b, $8981, $8985, $8989, $8993, $8995, $89a1,
  $89a2, $89a5, $89a8, $89a9, $89ab, $89ad, $89b0, $89b1, $89b3, $89b5, $89b7,
  $89b8, $89c1, $89c2, $89c5, $89c9, $89cb,
  $89d1, $89d3, $89d5, $89d7, $89e1, $89e5, $89e9, $89f1, $89f6, $89f7, $8a41,
  $8a42, $8a45, $8a49, $8a51, $8a53, $8a55, $8a57, $8a61, $8a65, $8a69, $8a73,
  $8a75, $8a81, $8a82, $8a85, $8a88, $8a89, $8a8a, $8a8b, $8a90, $8a91, $8a93,
  $8a95, $8a97, $8a98, $8aa1, $8aa2, $8aa5, $8aa9, $8ab6, $8ab7, $8ac1, $8ad5,
  $8ae1, $8ae2, $8ae5, $8ae9, $8af1, $8af3, $8af5, $8b41, $8b45, $8b49, $8b61,
  $8b62, $8b65, $8b68, $8b69, $8b6a, $8b71, $8b73, $8b75, $8b77, $8b81, $8ba1,
  $8ba2, $8ba5, $8ba8, $8ba9, $8bab, $8bb1, $8bb3, $8bb5, $8bb7, $8bb8, $8bbc,
  $8c61, $8c62, $8c63, $8c65, $8c69, $8c6b, $8c71, $8c73, $8c75, $8c76, $8c77,
  $8c7b, $8c81, $8c82, $8c85, $8c89, $8c91,
  $8c93, $8c95, $8c96, $8c97, $8ca1, $8ca2, $8ca9, $8ce1, $8ce2, $8ce3, $8ce5,
  $8ce9, $8cf1, $8cf3, $8cf5, $8cf6, $8cf7, $8d41, $8d42, $8d45, $8d51, $8d55,
  $8d57, $8d61, $8d65, $8d69, $8d75, $8d76, $8d7b, $8d81, $8da1, $8da2, $8da5,
  $8da7, $8da9, $8db1, $8db3, $8db5, $8db7, $8db8, $8db9, $8dc1, $8dc2, $8dc9,
  $8dd6, $8dd7, $8de1, $8de2, $8df7, $8e41, $8e45, $8e49, $8e51, $8e53, $8e57,
  $8e61, $8e81, $8e82, $8e85, $8e89, $8e90, $8e91, $8e93, $8e95, $8e97, $8e98,
  $8ea1, $8ea9, $8eb6, $8eb7, $8ec1, $8ec2, $8ec5, $8ec9, $8ed1, $8ed3, $8ed6,
  $8ee1, $8ee5, $8ee9, $8ef1, $8ef3, $8f41, $8f61, $8f62, $8f65, $8f67, $8f69,
  $8f6b, $8f70, $8f71, $8f73, $8f75, $8f77,
  $8f7b, $8fa1, $8fa2, $8fa5, $8fa9, $8fb1, $8fb3, $8fb5, $8fb7, $9061, $9062,
  $9063, $9065, $9068, $9069, $906a, $906b, $9071, $9073, $9075, $9076, $9077,
  $9078, $9079, $907b, $907d, $9081, $9082, $9085, $9089, $9091, $9093, $9095,
  $9096, $9097, $90a1, $90a2, $90a5, $90a9, $90b1, $90b7, $90e1, $90e2, $90e4,
  $90e5, $90e9, $90eb, $90ec, $90f1, $90f3, $90f5, $90f6, $90f7, $90fd, $9141,
  $9142, $9145, $9149, $9151, $9153, $9155, $9156, $9157, $9161, $9162, $9165,
  $9169, $9171, $9173, $9176, $9177, $917a, $9181, $9185, $91a1, $91a2, $91a5,
  $91a9, $91ab, $91b1, $91b3, $91b5, $91b7, $91bc, $91bd, $91c1, $91c5, $91c9,
  $91d6, $9241, $9245, $9249, $9251, $9253,
  $9255, $9261, $9262, $9265, $9269, $9273, $9275, $9277, $9281, $9282, $9285,
  $9288, $9289, $9291, $9293, $9295, $9297, $92a1, $92b6, $92c1, $92e1, $92e5,
  $92e9, $92f1, $92f3, $9341, $9342, $9349, $9351, $9353, $9357, $9361, $9362,
  $9365, $9369, $936a, $936b, $9371, $9373, $9375, $9377, $9378, $937c, $9381,
  $9385, $9389, $93a1, $93a2, $93a5, $93a9, $93af, $93b1, $93b3, $93b5, $93b7,
  $93bc, $9461, $9462, $9463, $9465, $9468, $9469, $946a, $946b, $946c, $9470,
  $9471, $9473, $9475, $9476, $9477, $9478, $9479, $947d, $9481, $9482, $9485,
  $9489, $9491, $9493, $9495, $9496, $9497, $94a1, $94e1, $94e2, $94e3, $94e5,
  $94e8, $94e9, $94eb, $94ec, $94f1, $94f3,
  $94f5, $94f7, $94f9, $94fc, $9541, $9542, $9545, $9549, $9551, $9553, $9555,
  $9556, $9557, $9561, $9565, $9569, $9576, $9577, $9581, $9585, $95a1, $95a2,
  $95a5, $95a8, $95a9, $95ab, $95ad, $95b1, $95b3, $95b5, $95b7, $95b9, $95bb,
  $95c1, $95c5, $95c9, $95e1, $95f6, $9641, $9645, $9649, $9651, $9653, $9655,
  $9661, $9681, $9682, $9685, $9689, $9691, $9693, $9695, $9697, $96a1, $96b6,
  $96c1, $96d7, $96e1, $96e5, $96e9, $96f3, $96f5, $96f7, $9741, $9745, $9749,
  $9751, $9757, $9761, $9762, $9765, $9768, $9769, $976b, $9771, $9773, $9775,
  $9777, $9781, $97a1, $97a2, $97a5, $97a8, $97a9, $97b1, $97b3, $97b5, $97b6,
  $97b7, $97b8, $9861, $9862, $9865, $9869,
  $9871, $9873, $9875, $9876, $9877, $987d, $9881, $9882, $9885, $9889, $9891,
  $9893, $9895, $9896, $9897, $98e1, $98e2, $98e5, $98e9, $98eb, $98ec, $98f1,
  $98f3, $98f5, $98f6, $98f7, $98fd, $9941, $9942, $9945, $9949, $9951, $9953,
  $9955, $9956, $9957, $9961, $9976, $99a1, $99a2, $99a5, $99a9, $99b7, $99c1,
  $99c9, $99e1, $9a41, $9a45, $9a81, $9a82, $9a85, $9a89, $9a90, $9a91, $9a97,
  $9ac1, $9ae1, $9ae5, $9ae9, $9af1, $9af3, $9af7, $9b61, $9b62, $9b65, $9b68,
  $9b69, $9b71, $9b73, $9b75, $9b81, $9b85, $9b89, $9b91, $9b93, $9ba1, $9ba5,
  $9ba9, $9bb1, $9bb3, $9bb5, $9bb7, $9c61, $9c62, $9c65, $9c69, $9c71, $9c73,
  $9c75, $9c76, $9c77, $9c78, $9c7c, $9c7d,
  $9c81, $9c82, $9c85, $9c89, $9c91, $9c93, $9c95, $9c96, $9c97, $9ca1, $9ca2,
  $9ca5, $9cb5, $9cb7, $9ce1, $9ce2, $9ce5, $9ce9, $9cf1, $9cf3, $9cf5, $9cf6,
  $9cf7, $9cfd, $9d41, $9d42, $9d45, $9d49, $9d51, $9d53, $9d55, $9d57, $9d61,
  $9d62, $9d65, $9d69, $9d71, $9d73, $9d75, $9d76, $9d77, $9d81, $9d85, $9d93,
  $9d95, $9da1, $9da2, $9da5, $9da9, $9db1, $9db3, $9db5, $9db7, $9dc1, $9dc5,
  $9dd7, $9df6, $9e41, $9e45, $9e49, $9e51, $9e53, $9e55, $9e57, $9e61, $9e65,
  $9e69, $9e73, $9e75, $9e77, $9e81, $9e82, $9e85, $9e89, $9e91, $9e93, $9e95,
  $9e97, $9ea1, $9eb6, $9ec1, $9ee1, $9ee2, $9ee5, $9ee9, $9ef1, $9ef5, $9ef7,
  $9f41, $9f42, $9f45, $9f49, $9f51, $9f53,
  $9f55, $9f57, $9f61, $9f62, $9f65, $9f69, $9f71, $9f73, $9f75, $9f77, $9f78,
  $9f7b, $9f7c, $9fa1, $9fa2, $9fa5, $9fa9, $9fb1, $9fb3, $9fb5, $9fb7, $a061,
  $a062, $a065, $a067, $a068, $a069, $a06a, $a06b, $a071, $a073, $a075, $a077,
  $a078, $a07b, $a07d, $a081, $a082, $a085, $a089, $a091, $a093, $a095, $a096,
  $a097, $a098, $a0a1, $a0a2, $a0a9, $a0b7, $a0e1, $a0e2, $a0e5, $a0e9, $a0eb,
  $a0f1, $a0f3, $a0f5, $a0f7, $a0f8, $a0fd, $a141, $a142, $a145, $a149, $a151,
  $a153, $a155, $a156, $a157, $a161, $a162, $a165, $a169, $a175, $a176, $a177,
  $a179, $a181, $a1a1, $a1a2, $a1a4, $a1a5, $a1a9, $a1ab, $a1b1, $a1b3, $a1b5,
  $a1b7, $a1c1, $a1c5, $a1d6, $a1d7, $a241,
  $a245, $a249, $a253, $a255, $a257, $a261, $a265, $a269, $a273, $a275, $a281,
  $a282, $a283, $a285, $a288, $a289, $a28a, $a28b, $a291, $a293, $a295, $a297,
  $a29b, $a29d, $a2a1, $a2a5, $a2a9, $a2b3, $a2b5, $a2c1, $a2e1, $a2e5, $a2e9,
  $a341, $a345, $a349, $a351, $a355, $a361, $a365, $a369, $a371, $a375, $a3a1,
  $a3a2, $a3a5, $a3a8, $a3a9, $a3ab, $a3b1, $a3b3, $a3b5, $a3b6, $a3b7, $a3b9,
  $a3bb, $a461, $a462, $a463, $a464, $a465, $a468, $a469, $a46a, $a46b, $a46c,
  $a471, $a473, $a475, $a477, $a47b, $a481, $a482, $a485, $a489, $a491, $a493,
  $a495, $a496, $a497, $a49b, $a4a1, $a4a2, $a4a5, $a4b3, $a4e1, $a4e2, $a4e5,
  $a4e8, $a4e9, $a4eb, $a4f1, $a4f3, $a4f5, $a4f7, $a4f8, $a541, $a542, $a545,
  $a548, $a549, $a551, $a553, $a555, $a556, $a557, $a561, $a562, $a565, $a569,
  $a573, $a575, $a576, $a577, $a57b, $a581, $a585, $a5a1, $a5a2, $a5a3, $a5a5,
  $a5a9, $a5b1, $a5b3, $a5b5, $a5b7, $a5c1, $a5c5, $a5d6, $a5e1, $a5f6, $a641,
  $a642, $a645, $a649, $a651, $a653, $a661, $a665, $a681, $a682, $a685, $a688,
  $a689, $a68a, $a68b, $a691, $a693, $a695, $a697, $a69b, $a69c, $a6a1, $a6a9,
  $a6b6, $a6c1, $a6e1, $a6e2, $a6e5, $a6e9, $a6f7, $a741, $a745, $a749, $a751,
  $a755, $a757, $a761, $a762, $a765, $a769, $a771, $a773, $a775, $a7a1, $a7a2,
  $a7a5, $a7a9, $a7ab, $a7b1, $a7b3, $a7b5, $a7b7, $a7b8, $a7b9, $a861, $a862,
  $a865, $a869, $a86b, $a871, $a873, $a875, $a876, $a877, $a87d,
  $a881, $a882, $a885, $a889, $a891, $a893, $a895, $a896, $a897, $a8a1, $a8a2,
  $a8b1, $a8e1, $a8e2, $a8e5, $a8e8, $a8e9, $a8f1, $a8f5, $a8f6, $a8f7, $a941,
  $a957, $a961, $a962, $a971, $a973, $a975, $a976, $a977, $a9a1, $a9a2, $a9a5,
  $a9a9, $a9b1, $a9b3, $a9b7, $aa41, $aa61, $aa77, $aa81, $aa82, $aa85, $aa89,
  $aa91, $aa95, $aa97, $ab41, $ab57, $ab61, $ab65, $ab69, $ab71, $ab73, $aba1,
  $aba2, $aba5, $aba9, $abb1, $abb3, $abb5, $abb7, $ac61, $ac62, $ac64, $ac65,
  $ac68, $ac69, $ac6a, $ac6b, $ac71, $ac73, $ac75, $ac76, $ac77, $ac7b, $ac81,
  $ac82, $ac85, $ac89, $ac91, $ac93, $ac95, $ac96, $ac97, $aca1,
  $aca2, $aca5, $aca9, $acb1, $acb3, $acb5, $acb7, $acc1, $acc5, $acc9, $acd1,
  $acd7, $ace1, $ace2, $ace3, $ace4, $ace5, $ace8, $ace9, $aceb, $acec, $acf1,
  $acf3, $acf5, $acf6, $acf7, $acfc, $ad41, $ad42, $ad45, $ad49, $ad51, $ad53,
  $ad55, $ad56, $ad57, $ad61, $ad62, $ad65, $ad69, $ad71, $ad73, $ad75, $ad76,
  $ad77, $ad81, $ad85, $ad89, $ad97, $ada1, $ada2, $ada3, $ada5, $ada9, $adab,
  $adb1, $adb3, $adb5, $adb7, $adbb, $adc1, $adc2, $adc5, $adc9, $add7, $ade1,
  $ade5, $ade9, $adf1, $adf5, $adf6, $ae41, $ae45, $ae49, $ae51, $ae53, $ae55,
  $ae61, $ae62, $ae65, $ae69, $ae71, $ae73, $ae75, $ae77, $ae81, $ae82, $ae85,
  $ae88, $ae89, $ae91, $ae93, $ae95, $ae97,
  $ae99, $ae9b, $ae9c, $aea1, $aeb6, $aec1, $aec2, $aec5, $aec9, $aed1, $aed7,
  $aee1, $aee2, $aee5, $aee9, $aef1, $aef3, $aef5, $aef7, $af41, $af42, $af49,
  $af51, $af55, $af57, $af61, $af62, $af65, $af69, $af6a, $af71, $af73, $af75,
  $af77, $afa1, $afa2, $afa5, $afa8, $afa9, $afb0, $afb1, $afb3, $afb5, $afb7,
  $afbc, $b061, $b062, $b064, $b065, $b069, $b071, $b073, $b076, $b077, $b07d,
  $b081, $b082, $b085, $b089, $b091, $b093, $b096, $b097, $b0b7, $b0e1, $b0e2,
  $b0e5, $b0e9, $b0eb, $b0f1, $b0f3, $b0f6, $b0f7, $b141, $b145, $b149, $b157,
  $b1a1, $b1a2, $b1a5, $b1a8, $b1a9, $b1ab, $b1b1, $b1b3, $b1b7, $b1c1, $b1c2,
  $b1c5, $b1d6, $b1e1, $b1f6, $b241, $b245,
  $b249, $b251, $b253, $b261, $b281, $b282, $b285, $b289, $b291, $b293, $b297,
  $b2a1, $b2b6, $b2c1, $b2e1, $b2e5, $b357, $b361, $b362, $b365, $b369, $b36b,
  $b370, $b371, $b373, $b381, $b385, $b389, $b391, $b3a1, $b3a2, $b3a5, $b3a9,
  $b3b1, $b3b3, $b3b5, $b3b7, $b461, $b462, $b465, $b466, $b467, $b469, $b46a,
  $b46b, $b470, $b471, $b473, $b475, $b476, $b477, $b47b, $b47c, $b481, $b482,
  $b485, $b489, $b491, $b493, $b495, $b496, $b497, $b4a1, $b4a2, $b4a5, $b4a9,
  $b4ac, $b4b1, $b4b3, $b4b5, $b4b7, $b4bb, $b4bd, $b4c1, $b4c5, $b4c9, $b4d3,
  $b4e1, $b4e2, $b4e5, $b4e6, $b4e8, $b4e9, $b4ea, $b4eb, $b4f1, $b4f3, $b4f4,
  $b4f5, $b4f6, $b4f7, $b4f8, $b4fa, $b4fc,
  $b541, $b542, $b545, $b549, $b551, $b553, $b555, $b557, $b561, $b562, $b563,
  $b565, $b569, $b56b, $b56c, $b571, $b573, $b574, $b575, $b576, $b577, $b57b,
  $b57c, $b57d, $b581, $b585, $b589, $b591, $b593, $b595, $b596, $b5a1, $b5a2,
  $b5a5, $b5a9, $b5aa, $b5ab, $b5ad, $b5b0, $b5b1, $b5b3, $b5b5, $b5b7, $b5b9,
  $b5c1, $b5c2, $b5c5, $b5c9, $b5d1, $b5d3, $b5d5, $b5d6, $b5d7, $b5e1, $b5e2,
  $b5e5, $b5f1, $b5f5, $b5f7, $b641, $b642, $b645, $b649, $b651, $b653, $b655,
  $b657, $b661, $b662, $b665, $b669, $b671, $b673, $b675, $b677, $b681, $b682,
  $b685, $b689, $b68a, $b68b, $b691, $b693, $b695, $b697, $b6a1, $b6a2, $b6a5,
  $b6a9, $b6b1, $b6b3, $b6b6, $b6b7, $b6c1,
  $b6c2, $b6c5, $b6c9, $b6d1, $b6d3, $b6d7, $b6e1, $b6e2, $b6e5, $b6e9, $b6f1,
  $b6f3, $b6f5, $b6f7, $b741, $b742, $b745, $b749, $b751, $b753, $b755, $b757,
  $b759, $b761, $b762, $b765, $b769, $b76f, $b771, $b773, $b775, $b777, $b778,
  $b779, $b77a, $b77b, $b77c, $b77d, $b781, $b785, $b789, $b791, $b795, $b7a1,
  $b7a2, $b7a5, $b7a9, $b7aa, $b7ab, $b7b0, $b7b1, $b7b3, $b7b5, $b7b6, $b7b7,
  $b7b8, $b7bc, $b861, $b862, $b865, $b867, $b868, $b869, $b86b, $b871, $b873,
  $b875, $b876, $b877, $b878, $b881, $b882, $b885, $b889, $b891, $b893, $b895,
  $b896, $b897, $b8a1, $b8a2, $b8a5, $b8a7, $b8a9, $b8b1, $b8b7, $b8c1, $b8c5,
  $b8c9, $b8e1, $b8e2, $b8e5, $b8e9, $b8eb,
  $b8f1, $b8f3, $b8f5, $b8f7, $b8f8, $b941, $b942, $b945, $b949, $b951, $b953,
  $b955, $b957, $b961, $b965, $b969, $b971, $b973, $b976, $b977, $b981, $b9a1,
  $b9a2, $b9a5, $b9a9, $b9ab, $b9b1, $b9b3, $b9b5, $b9b7, $b9b8, $b9b9, $b9bd,
  $b9c1, $b9c2, $b9c9, $b9d3, $b9d5, $b9d7, $b9e1, $b9f6, $b9f7, $ba41, $ba45,
  $ba49, $ba51, $ba53, $ba55, $ba57, $ba61, $ba62, $ba65, $ba77, $ba81, $ba82,
  $ba85, $ba89, $ba8a, $ba8b, $ba91, $ba93, $ba95, $ba97, $baa1, $bab6, $bac1,
  $bae1, $bae2, $bae5, $bae9, $baf1, $baf3, $baf5, $bb41, $bb45, $bb49, $bb51,
  $bb61, $bb62, $bb65, $bb69, $bb71, $bb73, $bb75, $bb77, $bba1, $bba2, $bba5,
  $bba8, $bba9, $bbab, $bbb1, $bbb3, $bbb5,
  $bbb7, $bbb8, $bbbb, $bbbc, $bc61, $bc62, $bc65, $bc67, $bc69, $bc6c, $bc71,
  $bc73, $bc75, $bc76, $bc77, $bc81, $bc82, $bc85, $bc89, $bc91, $bc93, $bc95,
  $bc96, $bc97, $bca1, $bca5, $bcb7, $bce1, $bce2, $bce5, $bce9, $bcf1, $bcf3,
  $bcf5, $bcf6, $bcf7, $bd41, $bd57, $bd61, $bd76, $bda1, $bda2, $bda5, $bda9,
  $bdb1, $bdb3, $bdb5, $bdb7, $bdb9, $bdc1, $bdc2, $bdc9, $bdd6, $bde1, $bdf6,
  $be41, $be45, $be49, $be51, $be53, $be77, $be81, $be82, $be85, $be89, $be91,
  $be93, $be97, $bea1, $beb6, $beb7, $bee1, $bf41, $bf61, $bf71, $bf75, $bf77,
  $bfa1, $bfa2, $bfa5, $bfa9, $bfb1, $bfb3, $bfb7, $bfb8, $bfbd, $c061, $c062,
  $c065, $c067, $c069, $c071, $c073, $c075,
  $c076, $c077, $c078, $c081, $c082, $c085, $c089, $c091, $c093, $c095, $c096,
  $c097, $c0a1, $c0a5, $c0a7, $c0a9, $c0b1, $c0b7, $c0e1, $c0e2, $c0e5, $c0e9,
  $c0f1, $c0f3, $c0f5, $c0f6, $c0f7, $c141, $c142, $c145, $c149, $c151, $c153,
  $c155, $c157, $c161, $c165, $c176, $c181, $c185, $c197, $c1a1, $c1a2, $c1a5,
  $c1a9, $c1b1, $c1b3, $c1b5, $c1b7, $c1c1, $c1c5, $c1c9, $c1d7, $c241, $c245,
  $c249, $c251, $c253, $c255, $c257, $c261, $c271, $c281, $c282, $c285, $c289,
  $c291, $c293, $c295, $c297, $c2a1, $c2b6, $c2c1, $c2c5, $c2e1, $c2e5, $c2e9,
  $c2f1, $c2f3, $c2f5, $c2f7, $c341, $c345, $c349, $c351, $c357, $c361, $c362,
  $c365, $c369, $c371, $c373, $c375, $c377,
  $c3a1, $c3a2, $c3a5, $c3a8, $c3a9, $c3aa, $c3b1, $c3b3, $c3b5, $c3b7, $c461,
  $c462, $c465, $c469, $c471, $c473, $c475, $c477, $c481, $c482, $c485, $c489,
  $c491, $c493, $c495, $c496, $c497, $c4a1, $c4a2, $c4b7, $c4e1, $c4e2, $c4e5,
  $c4e8, $c4e9, $c4f1, $c4f3, $c4f5, $c4f6, $c4f7, $c541, $c542, $c545, $c549,
  $c551, $c553, $c555, $c557, $c561, $c565, $c569, $c571, $c573, $c575, $c576,
  $c577, $c581, $c5a1, $c5a2, $c5a5, $c5a9, $c5b1, $c5b3, $c5b5, $c5b7, $c5c1,
  $c5c2, $c5c5, $c5c9, $c5d1, $c5d7, $c5e1, $c5f7, $c641, $c649, $c661, $c681,
  $c682, $c685, $c689, $c691, $c693, $c695, $c697, $c6a1, $c6a5, $c6a9, $c6b7,
  $c6c1, $c6d7, $c6e1, $c6e2, $c6e5, $c6e9,
  $c6f1, $c6f3, $c6f5, $c6f7, $c741, $c745, $c749, $c751, $c761, $c762, $c765,
  $c769, $c771, $c773, $c777, $c7a1, $c7a2, $c7a5, $c7a9, $c7b1, $c7b3, $c7b5,
  $c7b7, $c861, $c862, $c865, $c869, $c86a, $c871, $c873, $c875, $c876, $c877,
  $c881, $c882, $c885, $c889, $c891, $c893, $c895, $c896, $c897, $c8a1, $c8b7,
  $c8e1, $c8e2, $c8e5, $c8e9, $c8eb, $c8f1, $c8f3, $c8f5, $c8f6, $c8f7, $c941,
  $c942, $c945, $c949, $c951, $c953, $c955, $c957, $c961, $c965, $c976, $c981,
  $c985, $c9a1, $c9a2, $c9a5, $c9a9, $c9b1, $c9b3, $c9b5, $c9b7, $c9bc, $c9c1,
  $c9c5, $c9e1, $ca41, $ca45, $ca55, $ca57, $ca61, $ca81, $ca82, $ca85, $ca89,
  $ca91, $ca93, $ca95, $ca97, $caa1, $cab6,
  $cac1, $cae1, $cae2, $cae5, $cae9, $caf1, $caf3, $caf7, $cb41, $cb45, $cb49,
  $cb51, $cb57, $cb61, $cb62, $cb65, $cb68, $cb69, $cb6b, $cb71, $cb73, $cb75,
  $cb81, $cb85, $cb89, $cb91, $cb93, $cba1, $cba2, $cba5, $cba9, $cbb1, $cbb3,
  $cbb5, $cbb7, $cc61, $cc62, $cc63, $cc65, $cc69, $cc6b, $cc71, $cc73, $cc75,
  $cc76, $cc77, $cc7b, $cc81, $cc82, $cc85, $cc89, $cc91, $cc93, $cc95, $cc96,
  $cc97, $cca1, $cca2, $cce1, $cce2, $cce5, $cce9, $ccf1, $ccf3, $ccf5, $ccf6,
  $ccf7, $cd41, $cd42, $cd45, $cd49, $cd51, $cd53, $cd55, $cd57, $cd61, $cd65,
  $cd69, $cd71, $cd73, $cd76, $cd77, $cd81, $cd89, $cd93, $cd95, $cda1, $cda2,
  $cda5, $cda9, $cdb1, $cdb3, $cdb5, $cdb7,
  $cdc1, $cdd7, $ce41, $ce45, $ce61, $ce65, $ce69, $ce73, $ce75, $ce81, $ce82,
  $ce85, $ce88, $ce89, $ce8b, $ce91, $ce93, $ce95, $ce97, $cea1, $ceb7, $cee1,
  $cee5, $cee9, $cef1, $cef5, $cf41, $cf45, $cf49, $cf51, $cf55, $cf57, $cf61,
  $cf65, $cf69, $cf71, $cf73, $cf75, $cfa1, $cfa2, $cfa5, $cfa9, $cfb1, $cfb3,
  $cfb5, $cfb7, $d061, $d062, $d065, $d069, $d06e, $d071, $d073, $d075, $d077,
  $d081, $d082, $d085, $d089, $d091, $d093, $d095, $d096, $d097, $d0a1, $d0b7,
  $d0e1, $d0e2, $d0e5, $d0e9, $d0eb, $d0f1, $d0f3, $d0f5, $d0f7, $d141, $d142,
  $d145, $d149, $d151, $d153, $d155, $d157, $d161, $d162, $d165, $d169, $d171,
  $d173, $d175, $d176, $d177, $d181, $d185,
  $d189, $d193, $d1a1, $d1a2, $d1a5, $d1a9, $d1ae, $d1b1, $d1b3, $d1b5, $d1b7,
  $d1bb, $d1c1, $d1c2, $d1c5, $d1c9, $d1d5, $d1d7, $d1e1, $d1e2, $d1e5, $d1f5,
  $d1f7, $d241, $d242, $d245, $d249, $d253, $d255, $d257, $d261, $d265, $d269,
  $d273, $d275, $d281, $d282, $d285, $d289, $d28e, $d291, $d295, $d297, $d2a1,
  $d2a5, $d2a9, $d2b1, $d2b7, $d2c1, $d2c2, $d2c5, $d2c9, $d2d7, $d2e1, $d2e2,
  $d2e5, $d2e9, $d2f1, $d2f3, $d2f5, $d2f7, $d341, $d342, $d345, $d349, $d351,
  $d355, $d357, $d361, $d362, $d365, $d367, $d368, $d369, $d36a, $d371, $d373,
  $d375, $d377, $d37b, $d381, $d385, $d389, $d391, $d393, $d397, $d3a1, $d3a2,
  $d3a5, $d3a9, $d3b1, $d3b3, $d3b5, $d3b7);
  //한글 낱자 51자의 조합형 코드
  SingleTable : array[0..50]  of Word = (
  $8841, $8c41, $8444, $9041, $8446, $8447, $9441, $9841, $9c41, $844a, $844b,
  $844c, $844d, $844e, $844f, $8450, $a041, $a441, $a841, $8454, $ac41, $b041,
  $b441, $b841, $bc41, $c041, $c441, $c841, $cc41, $d041, $8461, $8481, $84a1,
  $84c1, $84e1, $8541, $8561, $8581, $85a1, $85c1, $85e1, $8641, $8661, $8681,
  $86a1, $86c1, $86e1, $8741, $8761, $8781, $87a1);

function bSearch(Code, Min, Max : Word ;Tbl : array of Word): Integer;
var Mid : Word;
begin
Mid := (Min + Max) div 2;
if Code = Tbl[Min] then Result := Min
else if Code = Tbl[Max] then Result := Max
else if Code = Tbl[Mid] then Result := Mid
else begin
  if (Tbl[Mid] > Code) and (Tbl[Min] < Code) then Result := bSearch(Code, Min+1, Mid-1, Tbl)
  else if (Tbl[Mid] < Code) and (Tbl[Max] > Code) then Result := bSearch(Code, Mid+1, Max-1, Tbl)
  else Result := -1;
  end;
end;

function KSSM2KS(code : Word): Word;
var  high, low, temp : Byte;
     index : Integer;
begin
high := Hi(code);
low := Lo(code);
index := bSearch(code, 0, 2349, KSTable);
//  한글 변환
if index >= 0 then begin
  Result := ((((index div 94)+$b0) shl 8)+(index mod 94) + $a1);
  Exit;
//  특수 문자 변환1
end else if (high >= $d9) and (high <= $de)  then begin
  if (low < $30) or ((low > $7e) and (low < $91)) then begin
    Result := 0;
    Exit;
    end;
  if low >= $a1 then temp := (high - $d9) * 2 + $a1 + 1
  else begin
    temp := (high - $d9) * 2 + $a1;
    if low <= $7e then low := low + $70 else low := low + ($70 - 18);
    end;
  Result := (temp shl 8) + low;
  Exit;
 //  특수 문자 변환2
end else if high = $d4 then begin
  if low < $80 then begin
    Result := 0;
    Exit;
    end;
  if low <= $dd then begin
    temp := $ad;
    low := low + $21;
  end  else begin
    temp := $ae;
    low := low - $3d;
    end;
  Result := (temp shl 8) + low;
  Exit;
//  한자 변환
end else if (high >= $e0) and (high <= $f9) then begin
  if (low < $30) or ((low > $7e) and (low < $91)) then begin
    Result :=  0;
    Exit;
    end;
  if low >= $a1 then temp := (high - $e0) * 2 + $ca + 1
  else begin
    temp := (high - $e0) * 2 + $ca;
    if low <= $7e then low := low + $70
    else low := low + ($70 - 18);
    end;
  Result := (temp shl 8) + low;
  Exit;
  end;
for index := 0 to 50 do begin
  if SingleTable[index] = code then begin
    Result := $a4a1 + index;
    Exit;
    end;
  end;
Result := 0;
end;

function KS2KSSM(Code : Word): Word;
var  high, low, temp : Byte;
     index, iMod, iRem : Integer;
begin
high := Hi(code);
low := Lo(code);
//  특수 문자 변환1
if (high >= $a1) and (high <= $ac) then begin
  if (low < $a1) or (low > $fe) then begin
    Result := 0;
    Exit;
    end;
  iMod := (high - $a1) shr 1;
  iRem := (high - $a1) and $01;
  if(iRem <> 0) then temp := low
  else begin
    temp := low - $70;
    if (temp > $7e) then temp := temp + 18;
    end;
  Result := ((iMod + $d9) shl 8) + temp;
  Exit;
//  특수 문자 변환2
end else if (high = $ad) then begin
  if (low < $a1) or (low > $fe) then begin
    Result := 0;
    Exit;
    end;
  temp := low - $21;
  Result := ($d4 shl 8) + temp;
  Exit;
//  특수 문자 변환3
end else if (high = $ae) then begin
  if (low < $a1) or (low > $c1) then begin
    Result := 0;
    Exit;
    end;
  temp := low + $3d;
  Result := ($d4 shl 8) + temp;
  Exit;
//  한글 변환
end else if (high >= $b0) and (high <= $c8) then begin
  if (low < $a1) or (low > $fe) then begin
    Result := 0;
    Exit;
    end;
  index := (high - $b0) * 94 + low - $a1;
  Result := KSTable[index];
  Exit;
//  한자 변환
end else if (high >= $ca) and (high <= $fd) then begin
  if (low < $a1) or (low > $fe) then begin
    Result := 0;
    Exit;
    end;
  iMod := (high - $ca) shr 1;
  iRem := (high - $ca) and $01;
  if (iRem <> 0) then temp := low
  else begin
    temp := low - $70;
    if (temp > $7e) then temp := temp + 18;
    end;
  Result := ((iMod + $e0) shl 8) + temp;
  Exit;
end;
index := ((code shr 8) - $b0) * 94 + (code and $00ff) - $a1;
if(index < 0) or (index >= 2350) then begin
  Result := 0;
  Exit;
  end;
Result := KSTable[index];
end;

function WanJoChg(Src : string; p_mode : Boolean):string;
Var
    I : Integer;
    Code : Word;
    sImsi : String;
begin
I := 1;
sImsi:= '';
While I <= Length(Src) do begin
  if Src[I] < #128 then sImsi := sImsi + Src[I]
  else begin
    Code := (Byte(Src[I]) shl 8) + Byte(Src[I+1]);
    if p_mode then Code := KSSM2KS(Code)
    else Code := KS2KSSM(Code);
    if Code = 0 then sImsi := sImsi + '  '
    else sImsi := sImsi + Chr(Hi(Code)) + Chr(Lo(Code));
    Inc(I);
    end;
  Inc(I);
  end;
Result := sImsi;
end;

begin
end.
