package dev.raegous.magicconch.audio.internals;

import org.bouncycastle.crypto.AsymmetricCipherKeyPair;
import org.bouncycastle.mls.codec.Credential;
import org.bouncycastle.mls.codec.KeyPackage;
import org.bouncycastle.mls.codec.MLSInputStream;
import org.bouncycastle.mls.codec.MLSMessage;
import org.bouncycastle.mls.codec.MLSOutputStream;
import org.bouncycastle.mls.codec.Proposal;
import org.bouncycastle.mls.crypto.MlsCipherSuite;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;

public final class DaveMlsTestHarness {
    private static final short DAVE_SUITE_ID = 0x0002;

    private DaveMlsTestHarness() {
    }

    public static ExternalSigner createExternalSigner(byte[] identity) throws Exception {
        MlsCipherSuite suite = MlsCipherSuite.getSuite(DAVE_SUITE_ID);
        AsymmetricCipherKeyPair keyPair = suite.generateSignatureKeyPair();
        byte[] publicKey = suite.serializeSignaturePublicKey(keyPair.getPublic());
        byte[] privateKey = suite.serializeSignaturePrivateKey(keyPair.getPrivate());
        DaveSupport.ExternalSender sender = new DaveSupport.ExternalSender(
            publicKey,
            new DaveSupport.Credential(1, identity)
        );

        return new ExternalSigner(suite, privateKey, sender);
    }

    public static final class ExternalSigner {
        private final MlsCipherSuite suite;
        private final byte[] privateKey;
        private final DaveSupport.ExternalSender sender;

        private ExternalSigner(MlsCipherSuite suite, byte[] privateKey, DaveSupport.ExternalSender sender) {
            this.suite = suite;
            this.privateKey = privateKey;
            this.sender = sender;
        }

        public DaveSupport.ExternalSender sender() {
            return sender;
        }

        public byte[] signedAddProposalBatch(String groupId, byte[] keyPackageMessage) throws Exception {
            byte[] publicProposal = signedAddProposal(groupId, keyPackageMessage);
            ByteArrayOutputStream vector = new ByteArrayOutputStream();
            vector.write(writeMlsVarint(publicProposal.length));
            vector.write(publicProposal);

            ByteArrayOutputStream batch = new ByteArrayOutputStream();
            batch.write(0);
            batch.write(writeMlsVarint(vector.size()));
            batch.write(vector.toByteArray());
            return batch.toByteArray();
        }

        public byte[] signedRawAddProposalBatch(String groupId, byte[] keyPackageMessage) throws Exception {
            byte[] publicProposal = signedAddProposal(groupId, keyPackageMessage);
            ByteArrayOutputStream batch = new ByteArrayOutputStream();
            batch.write(0);
            batch.write(writeMlsVarint(publicProposal.length));
            batch.write(publicProposal);
            return batch.toByteArray();
        }

        private byte[] signedAddProposal(String groupId, byte[] keyPackageMessage) throws Exception {
            KeyPackage keyPackage = parseKeyPackage(keyPackageMessage);
            Proposal proposal = Proposal.add(keyPackage);
            MLSMessage signedProposal = MLSMessage.externalProposal(
                suite,
                groupId.getBytes(StandardCharsets.UTF_8),
                0L,
                proposal,
                0,
                privateKey
            );

            return MLSOutputStream.encode(signedProposal);
        }
    }

    private static KeyPackage parseKeyPackage(byte[] keyPackageBytes) throws Exception {
        try {
            return (KeyPackage) MLSInputStream.decode(keyPackageBytes, KeyPackage.class);
        } catch (Exception ignored) {
            MLSMessage message = (MLSMessage) MLSInputStream.decode(keyPackageBytes, MLSMessage.class);
            if (message.keyPackage == null) {
                throw new IllegalArgumentException("MLS message did not contain a key package");
            }
            return message.keyPackage;
        }
    }

    private static byte[] writeMlsVarint(int value) {
        if (value < 0) {
            throw new IllegalArgumentException("MLS varint value must be non-negative: " + value);
        }
        if (value < 0x40) {
            return new byte[] { (byte) value };
        }
        if (value < 0x4000) {
            return new byte[] { (byte) ((value >> 8) | 0x40), (byte) value };
        }
        if (value < 0x40000000) {
            return new byte[] {
                (byte) ((value >> 24) | 0x80),
                (byte) (value >> 16),
                (byte) (value >> 8),
                (byte) value
            };
        }
        throw new IllegalArgumentException("MLS varint too large: " + value);
    }
}
